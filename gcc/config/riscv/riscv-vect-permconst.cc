/* Copyright (C) 2024 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or(at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#define IN_TARGET_CODE 1
#define INCLUDE_ALGORITHM
#define INCLUDE_FUNCTIONAL
#define INCLUDE_MEMORY

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "backend.h"
#include "rtl.h"
#include "target.h"
#include "tree-pass.h"
#include "df.h"
#include "rtl-ssa.h"
#include "cfgcleanup.h"
#include "insn-attr.h"
#include "tm-constrs.h"
#include "insn-opinit.h"
#include "cfgrtl.h"

/* So the basic idea of this pass is to identify loads of permutation
   constants from the constant pool which could instead be trivially
   derived from some earlier vector permutation constant.  This will
   replace a memory load from the constant pool with a vadd.vi
   instruction.

   Conceptually this is much like the related_values optimization in
   CSE, reload_cse_move2add or using SLSR to optimize constant synthesis.
   If we wanted to make this generic I would suggest putting it into CSE
   and providing target hooks to determine if particular permutation
   constants could be derived from earlier permutation constants.  */

const pass_data pass_data_vect_permconst = {
  RTL_PASS,	 /* type */
  "vect_permconst",	 /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_NONE,	 /* tv_id */
  0,		 /* properties_required */
  0,		 /* properties_provided */
  0,		 /* properties_destroyed */
  0,		 /* todo_flags_start */
  0,		 /* todo_flags_finish */
};

/* Entry in the hash table.  We "normalize" the permutation constant
   by adjusting all entries by the value in the first element.  This
   allows simple hashing to discover permutation constants that differ
   by a single constant across all their elements and may be derived
   from each other with a vadd.vi.  */

struct vector_permconst_entry
{
  /* The CONST_VECTOR in normalized form (first entry is zero).  */
  /* We could avoid copying the vector with a more customized hash
     routine which took care of normalization.  */
  rtx normalized_vec;

  /* The destination register holding the CONST_VECTOR.  When the optimization
     applies this will be used as a source operand in the vadd.vi.  */
  rtx dest;

  /* The insn generating DEST, the only reason we need this is because we
     do not invalidate entries which implies we have to verify that DEST
     is unchanged between INSN and the point where we want to use DEST
     to derive a new permutation constant.  */
  rtx_insn *insn;

  /* The bias of this entry used for normalization.  If this value is added
     to each element in NORMALIZED_VEC we would have the original permutation
     constant.  */
  HOST_WIDE_INT bias;
};

struct const_vector_hasher : nofree_ptr_hash <vector_permconst_entry>
{
  static inline hashval_t hash (const vector_permconst_entry *);
  static inline bool equal (const vector_permconst_entry *,
			    const vector_permconst_entry *);
};

inline bool
const_vector_hasher::equal (const vector_permconst_entry *vpe1,
			    const vector_permconst_entry *vpe2)
{
  /* Do the cheap tests first, namely that the mode and number of entries
     match between the two enries.  */
  if (GET_MODE (vpe1->normalized_vec) != GET_MODE (vpe2->normalized_vec))
    return false;

  if (CONST_VECTOR_NUNITS (vpe1->normalized_vec).to_constant ()
      != CONST_VECTOR_NUNITS (vpe2->normalized_vec).to_constant ())
    return false;

  /* Check the value of each entry in the vector.  We violate structure
     sharing rules inside this pass, so while pointer equality would normally
     be OK, it isn't here.  */
  for (int i = 0;
       i < CONST_VECTOR_NUNITS (vpe1->normalized_vec).to_constant ();
       i++)
    if (!rtx_equal_p (CONST_VECTOR_ELT (vpe1->normalized_vec, i),
		      CONST_VECTOR_ELT (vpe2->normalized_vec, i)))
      return false;

  return true;
}

inline hashval_t
const_vector_hasher::hash (const vector_permconst_entry *vpe)
{
  int do_not_record;
  return hash_rtx (vpe->normalized_vec, Pmode, &do_not_record, NULL, false);
}


class vector_permconst : public rtl_opt_pass
{
public:
  vector_permconst (gcc::context *ctxt)
    : rtl_opt_pass (pass_data_vect_permconst, ctxt) {}

  /* opt_pass methods: */
  virtual bool gate (function *) final override
  {
    return TARGET_VECTOR && optimize > 0;
  }
  virtual unsigned int execute (function *) final override;

private:
  void process_bb (basic_block);
  hash_table<const_vector_hasher> *vector_permconst_table;
}; // class pass_vector_permconst

/* Try to optimize vector permutation constants in BB.  */
void
vector_permconst::process_bb (basic_block bb)
{
  vector_permconst_table = new hash_table<const_vector_hasher> (11);

  /* Walk the insns in BB searching for vector loads from the constant pool
     which can be satisfied by adjusting an earlier load with trivial
     arithmetic.  */
  rtx_insn *insn;
  FOR_BB_INSNS (bb, insn)
    {
      if (!INSN_P (insn))
	continue;

      rtx set = single_set (insn);
      if (!set)
	continue;

      rtx dest = SET_DEST (set);
      if (GET_MODE_CLASS (GET_MODE (dest)) != MODE_VECTOR_INT)
	continue;

      rtx src = SET_SRC (set);
      if (!MEM_P (src))
	continue;

      /* A load from the constant pool should have a REG_EQUAL
	 note with the vector contant in the note.  */
      rtx note = find_reg_equal_equiv_note (insn);
      if (!note
	  || REG_NOTE_KIND (note) != REG_EQUAL
	  || GET_CODE (XEXP (note, 0)) != CONST_VECTOR)
	continue;

      if (!CONST_VECTOR_NUNITS (XEXP (note, 0)).is_constant ())
	continue;

      /* XXX Do we need to consider other forms of constants?  */

      /* We want to be selective about what gets past this point since
	 we make a copy of the vector and possibly enter it into the
	 hash table.  So reject cases that are not likely a permutation
	 constant.  ie, negative bias and large biases.  We arbitrarily
	 use 16k as the largest vector size in bits we try to optimize.

	 It may seem like a bias outside the range of vadd.vi should
	 be rejected, but what really matters is the difference of
	 biases across the two permutation constants.  */
      rtx cvec = XEXP (note, 0);
      HOST_WIDE_INT bias = INTVAL (CONST_VECTOR_ELT (cvec, 0));
      if (bias < 0 || bias > 16384 / 8)
	continue;

      /* We need to verify that each element would be a valid value
	 in the inner mode after applying the bias.  */
      machine_mode inner = GET_MODE_INNER (GET_MODE (cvec));
      HOST_WIDE_INT precision = GET_MODE_PRECISION (inner).to_constant ();
      int i;
      for (i = 0; i < CONST_VECTOR_NUNITS (cvec).to_constant (); i++)
	{
	  HOST_WIDE_INT val = INTVAL (CONST_VECTOR_ELT (cvec, i)) - bias;
	  if (val != sext_hwi (val, precision))
	    break;
	}

      /* If the loop terminated early, then we found a case where the
	 adjusted constant would not fit, so we can't record the constant
	 for this case (it's unlikely to be useful anyway.  */
      if (i != CONST_VECTOR_NUNITS (cvec).to_constant ())
	continue;

      /* At this point we have a load of a constant integer vector from the
	 constant pool.  That constant integer vector is hopefully a
	 permutation constant.  We need to make a copy of the vector and
	 normalize it to zero.

	 XXX This violates structure sharing conventions.  */
      rtvec_def *nvec = rtvec_alloc (CONST_VECTOR_NUNITS (cvec).to_constant ());

      for (i = 0; i < CONST_VECTOR_NUNITS (cvec).to_constant (); i++)
	nvec->elem[i] = GEN_INT (INTVAL (CONST_VECTOR_ELT (cvec, i)) - bias);

      rtx copy = gen_rtx_CONST_VECTOR (GET_MODE (cvec), nvec);

      /* Now that we have a normalized vector, look it up in the hash table,
	 inserting it if it wasn't already in the table.  */
      struct vector_permconst_entry tmp;
      tmp.normalized_vec = copy;
      struct vector_permconst_entry **slot
	= vector_permconst_table->find_slot (&tmp, INSERT);
      if (*slot == NULL)
	{
	  /* This constant was not in the table, so initialize the hash table
	     entry.  */
	  *slot = XNEW (vector_permconst_entry);
	  (*slot)->normalized_vec = copy;
	  (*slot)->dest = dest;
	  (*slot)->bias = bias;
	  (*slot)->insn = insn;
	}
      else
	{
	  /* A hit in the hash table.  We may be able to optimize this case.

	     If the difference in biases fits in the immediate range for
	     vadd.vi, then we may optimize.  */
	  HOST_WIDE_INT adjustment = bias - (*slot)->bias;
	  if (IN_RANGE (adjustment, -16, 15))
	    {
	      /* We also need to make sure the destination register was not
		 modified.  I've chosen to test for that at optimization time
		 rather than invalidate entries in the table.  This could be
		 changed to use REG_TICK like schemes or true invalidation if
		 this proves too compile-time costly.  */
	      if (!reg_set_between_p ((*slot)->dest, (*slot)->insn, insn))
		{
		  /* Instead of loading from the constant pool, adjust the
		     output of the earlier insn into our destination.  */
		  rtx x = gen_const_vec_duplicate (GET_MODE (copy),
						   GEN_INT (adjustment));
		  rtx plus = gen_rtx_PLUS (GET_MODE (copy), (*slot)->dest, x);
		  rtx set = gen_rtx_SET (dest, plus);
		  rtx_insn *new_insn = emit_insn_after (set, insn);
		  /* XXX Should we copy over the REG_EQUAL note first?  */
		  delete_insn (insn);
		  insn = new_insn;
		}
	    }

	  /* We always keep the hash table entry pointing to the most recent
	     INSN that could generate the normalized entry.  We can adjust
	     in the future if data says it's useful to do so.  This just
	     keeps things simple for now.

	     For example, we might want to keep multiple entries if they
	     have a different biases.  */
	  (*slot)->dest = dest;
	  (*slot)->bias = bias;
	  (*slot)->insn = insn;
	}
    }

  /* We construct and tear down the table for each block.  This may
     be overly expensive.  */
  vector_permconst_table->empty ();
}

/* Main entry point for this pass.  */
unsigned int
vector_permconst::execute (function *fn)
{
  /* Handle each block independently.  While this should work nicely on EBBs,
     let's wait for real world cases where it matters before adding that
     complexity.  */
  basic_block bb;
  FOR_EACH_BB_FN (bb, fn)
    process_bb (bb);

  return 0;
}

rtl_opt_pass *
make_pass_vector_permconst (gcc::context *ctxt)
{
  return new vector_permconst (ctxt);
}
