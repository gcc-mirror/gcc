/* Rematerialize pseudos values.
   Copyright (C) 2014-2020 Free Software Foundation, Inc.
   Contributed by Vladimir Makarov <vmakarov@redhat.com>.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.	If not see
<http://www.gnu.org/licenses/>.	 */

/* This code objective is to rematerialize spilled pseudo values.  To
   do this we calculate available insn candidates.  The candidate is
   available at some point if there is dominated set of insns with the
   same pattern, the insn inputs are not dying or modified on any path
   from the set, the outputs are not modified.

   The insns containing memory or spilled pseudos (except for the
   rematerialized pseudo) are not considered as such insns are not
   profitable in comparison with regular loads of spilled pseudo
   values.  That simplifies the implementation as we don't need to
   deal with memory aliasing.

   To speed up available candidate calculation, we calculate partially
   available candidates first and use them for initialization of the
   availability.  That is because (partial) availability sets are
   sparse.

   The rematerialization sub-pass could be improved further in the
   following ways:

   o We could make longer live ranges of inputs in the
     rematerialization candidates if their hard registers are not used
     for other purposes.  This could be complicated if we need to
     update BB live info information as LRA does not use
     DF-infrastructure for compile-time reasons.  This problem could
     be overcome if constrain making live ranges longer only in BB/EBB
     scope.
   o We could use cost-based decision to choose rematerialization insn
     (currently all insns without memory is can be used).
   o We could use other free hard regs for unused output pseudos in
     rematerialization candidates although such cases probably will
     be very rare.  */


#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "rtl.h"
#include "df.h"
#include "insn-config.h"
#include "regs.h"
#include "memmodel.h"
#include "ira.h"
#include "recog.h"
#include "lra.h"
#include "lra-int.h"
#include "function-abi.h"

/* Number of candidates for rematerialization.  */
static unsigned int cands_num;

/* Bitmap used for different calculations.  */
static bitmap_head temp_bitmap;

/* Registers accessed via subreg_p.  */
static bitmap_head subreg_regs;

typedef struct cand *cand_t;
typedef const struct cand *const_cand_t;

/* Insn candidates for rematerialization.  The candidate insn should
   have the following properies:
   o no any memory (as access to memory is non-profitable)
   o no INOUT regs (it means no non-paradoxical subreg of output reg)
   o one output spilled pseudo (or reload pseudo of a spilled pseudo)
   o all other pseudos are with assigned hard regs.  */
struct cand
{
  /* Index of the candidates in all_cands. */
  int index;
  /* Insn pseudo regno for rematerialization.  */
  int regno;
  /* The candidate insn.  */
  rtx_insn *insn;
  /* Non-negative if a reload pseudo is in the insn instead of the
     pseudo for rematerialization.  */
  int reload_regno;
  /* Number of the operand containing the regno or its reload
     regno.  */
  int nop;
  /* Next candidate for the same regno.  */
  cand_t next_regno_cand;
};

/* Vector containing all candidates.  */
static vec<cand_t> all_cands;
/* Map: insn -> candidate representing it.  It is null if the insn cannot
   be used for rematerialization.  */
static cand_t *insn_to_cand;
/* A secondary map, for candidates that involve two insns, where the
   second one makes the equivalence.  The candidate must not be used
   before seeing this activation insn.  */
static cand_t *insn_to_cand_activation;

/* Map regno -> candidates can be used for the regno
   rematerialization.  */
static cand_t *regno_cands;

/* Data about basic blocks used for the rematerialization
   sub-pass.  */
class remat_bb_data
{
public:
  /* Basic block about which the below data are.  */
  basic_block bb;
  /* Registers changed in the basic block: */
  bitmap_head changed_regs;
  /* Registers becoming dead in the BB.  */
  bitmap_head dead_regs;
  /* Cands present in the BB whose in/out regs are not changed after
     the cands occurence and are not dead (except the reload
     regno).  */
  bitmap_head gen_cands;
  bitmap_head livein_cands; /* cands whose inputs live at the BB start.  */
  bitmap_head pavin_cands; /* cands partially available at BB entry.  */
  bitmap_head pavout_cands; /* cands partially available at BB exit.  */
  bitmap_head avin_cands; /* cands available at the entry of the BB.  */
  bitmap_head avout_cands; /* cands available at the exit of the BB.  */
};

/* Array for all BB data.  Indexed by the corresponding BB index.  */
typedef class remat_bb_data *remat_bb_data_t;

/* Basic blocks for data flow problems -- all bocks except the special
   ones.  */
static bitmap_head all_blocks;

/* All basic block data are referred through the following array.  */
static remat_bb_data_t remat_bb_data;

/* Two small functions for access to the bb data.  */
static inline remat_bb_data_t
get_remat_bb_data (basic_block bb)
{
  return &remat_bb_data[(bb)->index];
}

static inline remat_bb_data_t
get_remat_bb_data_by_index (int index)
{
  return &remat_bb_data[index];
}



/* Hash table for the candidates.  Different insns (e.g. structurally
   the same insns or even insns with different unused output regs) can
   be represented by the same candidate in the table.  */
static htab_t cand_table;

/* Hash function for candidate CAND.  */
static hashval_t
cand_hash (const void *cand)
{
  const_cand_t c = (const_cand_t) cand;
  lra_insn_recog_data_t id = lra_get_insn_recog_data (c->insn);
  struct lra_static_insn_data *static_id = id->insn_static_data;
  int nops = static_id->n_operands;
  hashval_t hash = 0;

  for (int i = 0; i < nops; i++)
    if (i == c->nop)
      hash = iterative_hash_object (c->regno, hash);
    else if (static_id->operand[i].type == OP_IN)
      hash = iterative_hash_object (*id->operand_loc[i], hash);
  return hash;
}

/* Equal function for candidates CAND1 and CAND2.  They are equal if
   the corresponding candidate insns have the same code, the same
   regno for rematerialization, the same input operands.  */
static int
cand_eq_p (const void *cand1, const void *cand2)
{
  const_cand_t c1 = (const_cand_t) cand1;
  const_cand_t c2 = (const_cand_t) cand2;
  lra_insn_recog_data_t id1 = lra_get_insn_recog_data (c1->insn);
  lra_insn_recog_data_t id2 = lra_get_insn_recog_data (c2->insn);
  struct lra_static_insn_data *static_id1 = id1->insn_static_data;
  int nops = static_id1->n_operands;

  if (c1->regno != c2->regno
      || INSN_CODE (c1->insn) < 0
      || INSN_CODE (c1->insn) != INSN_CODE (c2->insn))
    return false;
  gcc_assert (c1->nop == c2->nop);
  for (int i = 0; i < nops; i++)
    if (i != c1->nop && static_id1->operand[i].type == OP_IN
	&& *id1->operand_loc[i] != *id2->operand_loc[i])
      return false;
  return true;
}

/* Insert candidate CAND into the table if it is not there yet.
   Return candidate which is in the table.  */
static cand_t
insert_cand (cand_t cand)
{
  void **entry_ptr;

  entry_ptr = htab_find_slot (cand_table, cand, INSERT);
  if (*entry_ptr == NULL)
    *entry_ptr = (void *) cand;
  return (cand_t) *entry_ptr;
}

/* Free candidate CAND memory.  */
static void
free_cand (void *cand)
{
  free (cand);
}

/* Initiate the candidate table.  */
static void
initiate_cand_table (void)
{
  cand_table = htab_create (8000, cand_hash, cand_eq_p,
			    (htab_del) free_cand);
}

/* Finish the candidate table.  */
static void
finish_cand_table (void)
{
  htab_delete (cand_table);
}



/* Return true if X contains memory or some UNSPEC.  We cannot just
   check insn operands as memory or unspec might be not an operand
   itself but contain an operand.  Insn with memory access is not
   profitable for rematerialization.  Rematerialization of UNSPEC
   might result in wrong code generation as the UNPEC effect is
   unknown (e.g. generating a label).  */
static bool
bad_for_rematerialization_p (rtx x)
{
  int i, j;
  const char *fmt;
  enum rtx_code code;

  if (MEM_P (x) || GET_CODE (x) == UNSPEC || GET_CODE (x) == UNSPEC_VOLATILE)
    return true;
  code = GET_CODE (x);
  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	{
	  if (bad_for_rematerialization_p (XEXP (x, i)))
	    return true;
	}
      else if (fmt[i] == 'E')
	{
	  for (j = XVECLEN (x, i) - 1; j >= 0; j--)
	    if (bad_for_rematerialization_p (XVECEXP (x, i, j)))
	      return true;
	}
    }
  return false;
}

/* If INSN cannot be used for rematerialization, return negative
   value.  If INSN can be considered as a candidate for
   rematerialization, return value which is the operand number of the
   pseudo for which the insn can be used for rematerialization.  Here
   we consider the insns without any memory, spilled pseudo (except
   for the rematerialization pseudo), or dying or unused regs.  */
static int
operand_to_remat (rtx_insn *insn)
{
  lra_insn_recog_data_t id = lra_get_insn_recog_data (insn);
  struct lra_static_insn_data *static_id = id->insn_static_data;
  struct lra_insn_reg *reg, *found_reg = NULL;

  /* Don't rematerialize insns which can change PC.  */
  if (JUMP_P (insn) || CALL_P (insn))
    return -1;
  /* First find a pseudo which can be rematerialized.  */
  for (reg = id->regs; reg != NULL; reg = reg->next)
    {
      /* True FRAME_POINTER_NEEDED might be because we cannot follow
	 changing sp offsets, e.g. alloca is used.  If the insn contains
	 stack pointer in such case, we cannot rematerialize it as we
	 cannot know sp offset at a rematerialization place.  */
      if (reg->regno == STACK_POINTER_REGNUM && frame_pointer_needed)
	return -1;
      else if (reg->type == OP_OUT && ! reg->subreg_p
	       && find_regno_note (insn, REG_UNUSED, reg->regno) == NULL)
	{
	  /* We permits only one spilled reg.  */
	  if (found_reg != NULL)
	    return -1;
	  found_reg = reg;
        }
      /* IRA calculates conflicts separately for subregs of two words
	 pseudo.  Even if the pseudo lives, e.g. one its subreg can be
	 used lately, another subreg hard register can be already used
	 for something else.  In such case, it is not safe to
	 rematerialize the insn.  */
      if (reg->regno >= FIRST_PSEUDO_REGISTER
	  && bitmap_bit_p (&subreg_regs, reg->regno))
	return -1;

      /* Don't allow hard registers to be rematerialized.  */
      if (reg->regno < FIRST_PSEUDO_REGISTER)
	return -1;
    }
  if (found_reg == NULL)
    return -1;
  if (found_reg->regno < FIRST_PSEUDO_REGISTER)
    return -1;
  if (bad_for_rematerialization_p (PATTERN (insn)))
    return -1;
  /* Check the other regs are not spilled. */
  for (reg = id->regs; reg != NULL; reg = reg->next)
    if (found_reg == reg)
      continue;
    else if (reg->type == OP_INOUT)
      return -1;
    else if (reg->regno >= FIRST_PSEUDO_REGISTER
	     && reg_renumber[reg->regno] < 0)
      /* Another spilled reg.  */
      return -1;
    else if (reg->type == OP_IN)
      {
	if (find_regno_note (insn, REG_DEAD, reg->regno) != NULL)
	  /* We don't want to make live ranges longer.  */
	  return -1;
	/* Check that there is no output reg as the input one.  */
	for (struct lra_insn_reg *reg2 = id->regs;
	     reg2 != NULL;
	     reg2 = reg2->next)
	  if (reg2->type == OP_OUT && reg->regno == reg2->regno)
	    return -1;
	if (reg->regno < FIRST_PSEUDO_REGISTER)
	  for (struct lra_insn_reg *reg2 = static_id->hard_regs;
	       reg2 != NULL;
	       reg2 = reg2->next)
	    if (reg2->type == OP_OUT
		&& reg->regno <= reg2->regno
		&& (reg2->regno
		    < (int) end_hard_regno (reg->biggest_mode, reg->regno)))
	      return -1;
      }
  /* Check hard coded insn registers.  */
  for (struct lra_insn_reg *reg = static_id->hard_regs;
       reg != NULL;
       reg = reg->next)
    if (reg->type == OP_INOUT)
      return -1;
    else if (reg->type == OP_IN)
      {
	/* Check that there is no output hard reg as the input
	   one.  */
	  for (struct lra_insn_reg *reg2 = static_id->hard_regs;
	       reg2 != NULL;
	       reg2 = reg2->next)
	    if (reg2->type == OP_OUT && reg->regno == reg2->regno)
		return -1;
      }
  /* Find the rematerialization operand.  */
  int nop = static_id->n_operands;
  for (int i = 0; i < nop; i++)
    if (REG_P (*id->operand_loc[i])
	&& (int) REGNO (*id->operand_loc[i]) == found_reg->regno)
      return i;
  return -1;
}

/* Create candidate for INSN with rematerialization operand NOP and
   REGNO.  Insert the candidate into the table and set up the
   corresponding INSN_TO_CAND element.  */
static void
create_cand (rtx_insn *insn, int nop, int regno, rtx_insn *activation = NULL)
{
  lra_insn_recog_data_t id = lra_get_insn_recog_data (insn);
  rtx reg = *id->operand_loc[nop];
  gcc_assert (REG_P (reg));
  int op_regno = REGNO (reg);
  gcc_assert (op_regno >= FIRST_PSEUDO_REGISTER);
  cand_t cand = XNEW (struct cand);
  cand->insn = insn;
  cand->nop = nop;
  cand->regno = regno;
  cand->reload_regno = op_regno == regno ? -1 : op_regno;
  gcc_assert (cand->regno >= 0);
  cand_t cand_in_table = insert_cand (cand);
  insn_to_cand[INSN_UID (insn)] = cand_in_table;
  if (cand != cand_in_table)
    free (cand);
  else
    {
      /* A new cand.  */
      cand->index = all_cands.length ();
      all_cands.safe_push (cand);
      cand->next_regno_cand = regno_cands[cand->regno];
      regno_cands[cand->regno] = cand;
    }
  if (activation)
    insn_to_cand_activation[INSN_UID (activation)] = cand_in_table;
}

/* Create rematerialization candidates (inserting them into the
   table).  */
static void
create_cands (void)
{
  rtx_insn *insn;
  struct potential_cand
  {
    rtx_insn *insn;
    int nop;
  };
  struct potential_cand *regno_potential_cand;

  /* Create candidates.  */
  regno_potential_cand = XCNEWVEC (struct potential_cand, max_reg_num ());
  for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
    if (NONDEBUG_INSN_P (insn))
      {
	lra_insn_recog_data_t id = lra_get_insn_recog_data (insn);
	int keep_regno = -1;
	rtx set = single_set (insn);
	int nop;

	/* See if this is an output reload for a previous insn.  */
	if (set != NULL
	    && REG_P (SET_SRC (set)) && REG_P (SET_DEST (set)))
	  {
	    rtx dstreg = SET_DEST (set);
	    int src_regno = REGNO (SET_SRC (set));
	    int dst_regno = REGNO (dstreg);
	    rtx_insn *insn2 = regno_potential_cand[src_regno].insn;

	    if (insn2 != NULL 
		&& dst_regno >= FIRST_PSEUDO_REGISTER
		&& reg_renumber[dst_regno] < 0
		&& BLOCK_FOR_INSN (insn2) == BLOCK_FOR_INSN (insn))
	      {
		create_cand (insn2, regno_potential_cand[src_regno].nop,
			     dst_regno, insn);
		goto done;
	      }
	  }

	nop = operand_to_remat (insn);
	if (nop >= 0)
	  {
	    gcc_assert (REG_P (*id->operand_loc[nop]));
	    int regno = REGNO (*id->operand_loc[nop]);
	    gcc_assert (regno >= FIRST_PSEUDO_REGISTER);
	    /* If we're setting an unrenumbered pseudo, make a candidate immediately.
	       If it's an output reload register, save it for later; the code above
	       looks for output reload insns later on.  */
	    if (reg_renumber[regno] < 0)
	      create_cand (insn, nop, regno);
	    else if (regno >= lra_constraint_new_regno_start)
	      {
		regno_potential_cand[regno].insn = insn;
		regno_potential_cand[regno].nop = nop;
		keep_regno = regno;
	      }
	  }

      done:
	for (struct lra_insn_reg *reg = id->regs; reg != NULL; reg = reg->next)
	  if (reg->type != OP_IN && reg->regno != keep_regno
	      && reg->regno >= FIRST_PSEUDO_REGISTER)
	    regno_potential_cand[reg->regno].insn = NULL;
      }
  cands_num = all_cands.length ();
  free (regno_potential_cand);
}



/* Create and initialize BB data.  */
static void
create_remat_bb_data (void)
{
  basic_block bb;
  remat_bb_data_t bb_info;

  remat_bb_data = XNEWVEC (class remat_bb_data,
			   last_basic_block_for_fn (cfun));
  FOR_ALL_BB_FN (bb, cfun)
    {
      gcc_checking_assert (bb->index >= 0
			   && bb->index < last_basic_block_for_fn (cfun));
      bb_info = get_remat_bb_data (bb);
      bb_info->bb = bb;
      bitmap_initialize (&bb_info->changed_regs, &reg_obstack);
      bitmap_initialize (&bb_info->dead_regs, &reg_obstack);
      bitmap_initialize (&bb_info->gen_cands, &reg_obstack);
      bitmap_initialize (&bb_info->livein_cands, &reg_obstack);
      bitmap_initialize (&bb_info->pavin_cands, &reg_obstack);
      bitmap_initialize (&bb_info->pavout_cands, &reg_obstack);
      bitmap_initialize (&bb_info->avin_cands, &reg_obstack);
      bitmap_initialize (&bb_info->avout_cands, &reg_obstack);
    }
}

/* Dump all candidates to DUMP_FILE.  */
static void
dump_cands (FILE *dump_file)
{
  int i;
  cand_t cand;

  fprintf (dump_file, "\nCands:\n");
  for (i = 0; i < (int) cands_num; i++)
    {
      cand = all_cands[i];
      fprintf (dump_file, "%d (nop=%d, remat_regno=%d, reload_regno=%d):\n",
	       i, cand->nop, cand->regno, cand->reload_regno);
      print_inline_rtx (dump_file, cand->insn, 6);
      fprintf (dump_file, "\n");
    }
}

/* Dump all candidates and BB data.  */
static void
dump_candidates_and_remat_bb_data (void)
{
  basic_block bb;

  if (lra_dump_file == NULL)
    return;
  dump_cands (lra_dump_file);
  FOR_EACH_BB_FN (bb, cfun)
    {
      fprintf (lra_dump_file, "\nBB %d:\n", bb->index);
      /* Livein */
      fprintf (lra_dump_file, "  register live in:");
      dump_regset (df_get_live_in (bb), lra_dump_file);
      putc ('\n', lra_dump_file);
      /* Liveout */
      fprintf (lra_dump_file, "  register live out:");
      dump_regset (df_get_live_out (bb), lra_dump_file);
      putc ('\n', lra_dump_file);
      /* Changed/dead regs: */
      fprintf (lra_dump_file, "  changed regs:");
      dump_regset (&get_remat_bb_data (bb)->changed_regs, lra_dump_file);
      putc ('\n', lra_dump_file);
      fprintf (lra_dump_file, "  dead regs:");
      dump_regset (&get_remat_bb_data (bb)->dead_regs, lra_dump_file);
      putc ('\n', lra_dump_file);
      lra_dump_bitmap_with_title ("cands generated in BB",
				  &get_remat_bb_data (bb)->gen_cands, bb->index);
      lra_dump_bitmap_with_title ("livein cands in BB",
				  &get_remat_bb_data (bb)->livein_cands, bb->index);
      lra_dump_bitmap_with_title ("pavin cands in BB",
				  &get_remat_bb_data (bb)->pavin_cands, bb->index);
      lra_dump_bitmap_with_title ("pavout cands in BB",
				  &get_remat_bb_data (bb)->pavout_cands, bb->index);
      lra_dump_bitmap_with_title ("avin cands in BB",
				  &get_remat_bb_data (bb)->avin_cands, bb->index);
      lra_dump_bitmap_with_title ("avout cands in BB",
				  &get_remat_bb_data (bb)->avout_cands, bb->index);
    }
  fprintf (lra_dump_file, "subreg regs:");
  dump_regset (&subreg_regs, lra_dump_file);
  putc ('\n', lra_dump_file);
}

/* Free all BB data.  */
static void
finish_remat_bb_data (void)
{
  basic_block bb;

  FOR_EACH_BB_FN (bb, cfun)
    {
      bitmap_clear (&get_remat_bb_data (bb)->avout_cands);
      bitmap_clear (&get_remat_bb_data (bb)->avin_cands);
      bitmap_clear (&get_remat_bb_data (bb)->pavout_cands);
      bitmap_clear (&get_remat_bb_data (bb)->pavin_cands);
      bitmap_clear (&get_remat_bb_data (bb)->livein_cands);
      bitmap_clear (&get_remat_bb_data (bb)->gen_cands);
      bitmap_clear (&get_remat_bb_data (bb)->dead_regs);
      bitmap_clear (&get_remat_bb_data (bb)->changed_regs);
    }
  free (remat_bb_data);
}



/* Update changed_regs, dead_regs, subreg_regs of BB from INSN.  */
static void
set_bb_regs (basic_block bb, rtx_insn *insn)
{
  lra_insn_recog_data_t id = lra_get_insn_recog_data (insn);
  remat_bb_data_t bb_info = get_remat_bb_data (bb);
  struct lra_insn_reg *reg;

  for (reg = id->regs; reg != NULL; reg = reg->next)
    {
      unsigned regno = reg->regno;
      if (reg->type != OP_IN)
        bitmap_set_bit (&bb_info->changed_regs, regno);
      else if (find_regno_note (insn, REG_DEAD, regno) != NULL)
	bitmap_set_bit (&bb_info->dead_regs, regno);
      if (regno >= FIRST_PSEUDO_REGISTER && reg->subreg_p)
	bitmap_set_bit (&subreg_regs, regno);
    }
  if (CALL_P (insn))
    {
      /* Partially-clobbered registers might still be live.  */
      HARD_REG_SET clobbers = insn_callee_abi (insn).full_reg_clobbers ();
      bitmap_ior_into (&get_remat_bb_data (bb)->dead_regs,
		       bitmap_view<HARD_REG_SET> (clobbers));
    }
}

/* Calculate changed_regs and dead_regs for each BB.  */
static void
calculate_local_reg_remat_bb_data (void)
{
  basic_block bb;
  rtx_insn *insn;

  FOR_EACH_BB_FN (bb, cfun)
    FOR_BB_INSNS (bb, insn)
      if (NONDEBUG_INSN_P (insn))
	set_bb_regs (bb, insn);
}



/* Return true if REG overlaps an input operand of INSN.  */
static bool
reg_overlap_for_remat_p (lra_insn_reg *reg, rtx_insn *insn)
{
  int iter;
  lra_insn_recog_data_t id = lra_get_insn_recog_data (insn);
  struct lra_static_insn_data *static_id = id->insn_static_data;
  unsigned regno = reg->regno;
  int nregs;

  if (regno >= FIRST_PSEUDO_REGISTER && reg_renumber[regno] >= 0)
    regno = reg_renumber[regno];
  if (regno >= FIRST_PSEUDO_REGISTER)
    nregs = 1;
  else
    nregs = hard_regno_nregs (regno, reg->biggest_mode);

  struct lra_insn_reg *reg2;

  for (iter = 0; iter < 2; iter++)
    for (reg2 = (iter == 0 ? id->regs : static_id->hard_regs);
	 reg2 != NULL;
	 reg2 = reg2->next)
      {
	if (reg2->type != OP_IN)
	  continue;
	unsigned regno2 = reg2->regno;
	int nregs2;

	if (regno2 >= FIRST_PSEUDO_REGISTER && reg_renumber[regno2] >= 0)
	  regno2 = reg_renumber[regno2];
	if (regno2 >= FIRST_PSEUDO_REGISTER)
	  nregs2 = 1;
	else
	  nregs2 = hard_regno_nregs (regno2, reg->biggest_mode);

	if ((regno2 + nregs2 - 1 >= regno && regno2 < regno + nregs)
	    || (regno + nregs - 1 >= regno2 && regno < regno2 + nregs2))
	  return true;
      }
  return false;
}

/* Return true if a call used register is an input operand of INSN.  */
static bool
call_used_input_regno_present_p (const function_abi &abi, rtx_insn *insn)
{
  int iter;
  lra_insn_recog_data_t id = lra_get_insn_recog_data (insn);
  struct lra_static_insn_data *static_id = id->insn_static_data;
  struct lra_insn_reg *reg;

  for (iter = 0; iter < 2; iter++)
    for (reg = (iter == 0 ? id->regs : static_id->hard_regs);
	 reg != NULL;
	 reg = reg->next)
      if (reg->type == OP_IN
	  && reg->regno < FIRST_PSEUDO_REGISTER
	  && abi.clobbers_reg_p (reg->biggest_mode, reg->regno))
	return true;
  return false;
}

/* Calculate livein_cands for each BB.  */
static void
calculate_livein_cands (void)
{
  basic_block bb;

  FOR_EACH_BB_FN (bb, cfun)
    {
      bitmap livein_regs = df_get_live_in (bb);
      bitmap livein_cands = &get_remat_bb_data (bb)->livein_cands;
      for (unsigned int i = 0; i < cands_num; i++)
	{
	  cand_t cand = all_cands[i];
	  lra_insn_recog_data_t id = lra_get_insn_recog_data (cand->insn);
	  struct lra_insn_reg *reg;

	  for (reg = id->regs; reg != NULL; reg = reg->next)
	    if (reg->type == OP_IN && ! bitmap_bit_p (livein_regs, reg->regno))
	      break;
	  if (reg == NULL)
	    bitmap_set_bit (livein_cands, i);
	}
    }
}

/* Calculate gen_cands for each BB.  */
static void
calculate_gen_cands (void)
{
  basic_block bb;
  bitmap gen_cands;
  rtx_insn *insn;

  FOR_EACH_BB_FN (bb, cfun)
    {
      gen_cands = &get_remat_bb_data (bb)->gen_cands;
      auto_bitmap gen_insns (&reg_obstack);
      FOR_BB_INSNS (bb, insn)
	if (INSN_P (insn))
	  {
	    lra_insn_recog_data_t id = lra_get_insn_recog_data (insn);
	    struct lra_static_insn_data *static_id = id->insn_static_data;
	    struct lra_insn_reg *reg;
	    unsigned int uid;
	    bitmap_iterator bi;
	    cand_t cand;
	    rtx set;
	    int iter;
	    int src_regno = -1, dst_regno = -1;

	    if ((set = single_set (insn)) != NULL
		&& REG_P (SET_SRC (set)) && REG_P (SET_DEST (set)))
	      {
		src_regno = REGNO (SET_SRC (set));
		dst_regno = REGNO (SET_DEST (set));
	      }

	    /* Update gen_cands:  */
	    bitmap_clear (&temp_bitmap);
	    for (iter = 0; iter < 2; iter++)
	      for (reg = (iter == 0 ? id->regs : static_id->hard_regs);
		   reg != NULL;
		   reg = reg->next)
		if (reg->type != OP_IN
		    || find_regno_note (insn, REG_DEAD, reg->regno) != NULL)
		  EXECUTE_IF_SET_IN_BITMAP (gen_insns, 0, uid, bi)
		    {
		      rtx_insn *insn2 = lra_insn_recog_data[uid]->insn;
		      
		      cand = insn_to_cand[INSN_UID (insn2)];
		      gcc_assert (cand != NULL);
		      /* Ignore the reload insn.  */
		      if (src_regno == cand->reload_regno
			  && dst_regno == cand->regno)
			continue;
		      if (cand->regno == reg->regno
			  || reg_overlap_for_remat_p (reg, insn2))
			{
			  bitmap_clear_bit (gen_cands, cand->index);
			  bitmap_set_bit (&temp_bitmap, uid);
			}
		    }
	    
	    if (CALL_P (insn))
	      {
		function_abi callee_abi = insn_callee_abi (insn);
		EXECUTE_IF_SET_IN_BITMAP (gen_insns, 0, uid, bi)
		  {
		    rtx_insn *insn2 = lra_insn_recog_data[uid]->insn;
		  
		    cand = insn_to_cand[INSN_UID (insn2)];
		    gcc_assert (cand != NULL);
		    if (call_used_input_regno_present_p (callee_abi, insn2))
		      {
			bitmap_clear_bit (gen_cands, cand->index);
			bitmap_set_bit (&temp_bitmap, uid);
		      }
		  }
	      }
	    bitmap_and_compl_into (gen_insns, &temp_bitmap);

	    cand = insn_to_cand[INSN_UID (insn)];
	    if (cand != NULL)
	      {
		bitmap_set_bit (gen_cands, cand->index);
		bitmap_set_bit (gen_insns, INSN_UID (insn));
	      }
	  }
    }  
}



/* The common transfer function used by the DF equation solver to
   propagate (partial) availability info BB_IN to BB_OUT through block
   with BB_INDEX according to the following equation:

      bb.out =  ((bb.in & bb.livein) - bb.killed) OR  bb.gen
*/
static bool
cand_trans_fun (int bb_index, bitmap bb_in, bitmap bb_out)
{
  remat_bb_data_t bb_info;
  bitmap bb_livein, bb_changed_regs, bb_dead_regs;
  unsigned int cid;
  bitmap_iterator bi;

  bb_info = get_remat_bb_data_by_index (bb_index);
  bb_livein = &bb_info->livein_cands;
  bb_changed_regs = &bb_info->changed_regs;
  bb_dead_regs = &bb_info->dead_regs;
  /* Calculate killed avin cands -- cands whose regs are changed or
     becoming dead in the BB.  We calculate it here as we hope that
     repeated calculations are compensated by smaller size of BB_IN in
     comparison with all candidates number.  */
  bitmap_clear (&temp_bitmap);
  EXECUTE_IF_SET_IN_BITMAP (bb_in, 0, cid, bi)
    {
      cand_t cand = all_cands[cid];
      lra_insn_recog_data_t id = lra_get_insn_recog_data (cand->insn);
      struct lra_insn_reg *reg;

      if (! bitmap_bit_p (bb_livein, cid))
	{
	  bitmap_set_bit (&temp_bitmap, cid);
	  continue;
	}
      for (reg = id->regs; reg != NULL; reg = reg->next)
	/* Ignore all outputs which are not the regno for
	   rematerialization.  */
	if (reg->type == OP_OUT && reg->regno != cand->regno)
	  continue;
	else if (bitmap_bit_p (bb_changed_regs, reg->regno)
		 || bitmap_bit_p (bb_dead_regs, reg->regno))
	  {
	    bitmap_set_bit (&temp_bitmap, cid);
	    break;
	  }
      /* Check regno for rematerialization.  */
      if (bitmap_bit_p (bb_changed_regs, cand->regno)
	  || bitmap_bit_p (bb_dead_regs, cand->regno))
	bitmap_set_bit (&temp_bitmap, cid);
    }
  return bitmap_ior_and_compl (bb_out,
			       &bb_info->gen_cands, bb_in, &temp_bitmap);
}



/* The transfer function used by the DF equation solver to propagate
   partial candidate availability info through block with BB_INDEX
   according to the following equation:

     bb.pavout = ((bb.pavin & bb.livein) - bb.killed) OR bb.gen
*/
static bool
cand_pav_trans_fun (int bb_index)
{
  remat_bb_data_t bb_info;

  bb_info = get_remat_bb_data_by_index (bb_index);
  return cand_trans_fun (bb_index, &bb_info->pavin_cands,
			 &bb_info->pavout_cands);
}

/* The confluence function used by the DF equation solver to set up
   cand_pav info for a block BB without predecessor.  */
static void
cand_pav_con_fun_0 (basic_block bb)
{
  bitmap_clear (&get_remat_bb_data (bb)->pavin_cands);
}

/* The confluence function used by the DF equation solver to propagate
   partial candidate availability info from predecessor to successor
   on edge E (pred->bb) according to the following equation:

      bb.pavin_cands = 0 for entry block | OR (pavout_cands of predecessors)
 */
static bool
cand_pav_con_fun_n (edge e)
{
  basic_block pred = e->src;
  basic_block bb = e->dest;
  remat_bb_data_t bb_info;
  bitmap bb_pavin, pred_pavout;
  
  bb_info = get_remat_bb_data (bb);
  bb_pavin = &bb_info->pavin_cands;
  pred_pavout = &get_remat_bb_data (pred)->pavout_cands;
  return bitmap_ior_into (bb_pavin, pred_pavout);
}



/* The transfer function used by the DF equation solver to propagate
   candidate availability info through block with BB_INDEX according
   to the following equation:

      bb.avout =  ((bb.avin & bb.livein) - bb.killed) OR  bb.gen
*/
static bool
cand_av_trans_fun (int bb_index)
{
  remat_bb_data_t bb_info;

  bb_info = get_remat_bb_data_by_index (bb_index);
  return cand_trans_fun (bb_index, &bb_info->avin_cands,
			 &bb_info->avout_cands);
}

/* The confluence function used by the DF equation solver to set up
   cand_av info for a block BB without predecessor.  */
static void
cand_av_con_fun_0 (basic_block bb)
{
  bitmap_clear (&get_remat_bb_data (bb)->avin_cands);
}

/* The confluence function used by the DF equation solver to propagate
   cand_av info from predecessor to successor on edge E (pred->bb)
   according to the following equation:

      bb.avin_cands = 0 for entry block | AND (avout_cands of predecessors)
 */
static bool
cand_av_con_fun_n (edge e)
{
  basic_block pred = e->src;
  basic_block bb = e->dest;
  remat_bb_data_t bb_info;
  bitmap bb_avin, pred_avout;
  
  bb_info = get_remat_bb_data (bb);
  bb_avin = &bb_info->avin_cands;
  pred_avout = &get_remat_bb_data (pred)->avout_cands;
  return bitmap_and_into (bb_avin, pred_avout);
}

/* Calculate available candidates for each BB.  */
static void
calculate_global_remat_bb_data (void)
{
  basic_block bb;

  df_simple_dataflow
    (DF_FORWARD, NULL, cand_pav_con_fun_0, cand_pav_con_fun_n,
     cand_pav_trans_fun, &all_blocks,
     df_get_postorder (DF_FORWARD), df_get_n_blocks (DF_FORWARD));
  /* Initialize avin by pavin.  */
  FOR_EACH_BB_FN (bb, cfun)
    bitmap_copy (&get_remat_bb_data (bb)->avin_cands,
		 &get_remat_bb_data (bb)->pavin_cands);
  df_simple_dataflow
    (DF_FORWARD, NULL, cand_av_con_fun_0, cand_av_con_fun_n,
     cand_av_trans_fun, &all_blocks,
     df_get_postorder (DF_FORWARD), df_get_n_blocks (DF_FORWARD));
}



/* Setup sp offset attribute to SP_OFFSET for all INSNS.  */
static void
change_sp_offset (rtx_insn *insns, poly_int64 sp_offset)
{
  for (rtx_insn *insn = insns; insn != NULL; insn = NEXT_INSN (insn))
    eliminate_regs_in_insn (insn, false, false, sp_offset);
}

/* Return start hard register of REG (can be a hard or a pseudo reg)
   or -1 (if it is a spilled pseudo).  Return number of hard registers
   occupied by REG through parameter NREGS if the start hard reg is
   not negative.  */
static int
get_hard_regs (struct lra_insn_reg *reg, int &nregs)
{
  int regno = reg->regno;
  int hard_regno = regno < FIRST_PSEUDO_REGISTER ? regno : reg_renumber[regno];

  if (hard_regno >= 0)
    nregs = hard_regno_nregs (hard_regno, reg->biggest_mode);
  return hard_regno;
}

/* Make copy of and register scratch pseudos in rematerialized insn
   REMAT_INSN.  */
static void
update_scratch_ops (rtx_insn *remat_insn)
{
  lra_insn_recog_data_t id = lra_get_insn_recog_data (remat_insn);
  struct lra_static_insn_data *static_id = id->insn_static_data;
  for (int i = 0; i < static_id->n_operands; i++)
    {
      rtx *loc = id->operand_loc[i];
      if (! REG_P (*loc))
	continue;
      int regno = REGNO (*loc);
      if (! lra_former_scratch_p (regno))
	continue;
      *loc = lra_create_new_reg (GET_MODE (*loc), *loc,
				 lra_get_allocno_class (regno),
				 "scratch pseudo copy");
      lra_register_new_scratch_op (remat_insn, i, id->icode);
    }
  
}

/* Insert rematerialization insns using the data-flow data calculated
   earlier.  */
static bool
do_remat (void)
{
  unsigned regno;
  rtx_insn *insn;
  basic_block bb;
  bool changed_p = false;
  /* Living hard regs and hard registers of living pseudos.  */
  HARD_REG_SET live_hard_regs;
  bitmap_iterator bi;

  auto_bitmap avail_cands (&reg_obstack);
  auto_bitmap active_cands (&reg_obstack);
  FOR_EACH_BB_FN (bb, cfun)
    {
      CLEAR_HARD_REG_SET (live_hard_regs);
      EXECUTE_IF_SET_IN_BITMAP (df_get_live_in (bb), 0, regno, bi)
	{
	  int hard_regno = regno < FIRST_PSEUDO_REGISTER
			   ? regno
			   : reg_renumber[regno];
	  if (hard_regno >= 0)
	    SET_HARD_REG_BIT (live_hard_regs, hard_regno);
	}
      bitmap_and (avail_cands, &get_remat_bb_data (bb)->avin_cands,
		  &get_remat_bb_data (bb)->livein_cands);
      /* Activating insns are always in the same block as their corresponding
	 remat insn, so at the start of a block the two bitsets are equal.  */
      bitmap_copy (active_cands, avail_cands);
      FOR_BB_INSNS (bb, insn)
	{
	  if (!NONDEBUG_INSN_P (insn))
	    continue;

	  lra_insn_recog_data_t id = lra_get_insn_recog_data (insn);
	  struct lra_static_insn_data *static_id = id->insn_static_data;
	  struct lra_insn_reg *reg;
	  cand_t cand;
	  unsigned int cid;
	  bitmap_iterator bi;
	  rtx set;
	  int iter;
	  int src_regno = -1, dst_regno = -1;

	  if ((set = single_set (insn)) != NULL
	      && REG_P (SET_SRC (set)) && REG_P (SET_DEST (set)))
	    {
	      src_regno = REGNO (SET_SRC (set));
	      dst_regno = REGNO (SET_DEST (set));
	    }

	  cand = NULL;
	  /* Check possibility of rematerialization (hard reg or
	     unpsilled pseudo <- spilled pseudo): */
	  if (dst_regno >= 0 && src_regno >= FIRST_PSEUDO_REGISTER
	      && reg_renumber[src_regno] < 0
	      && (dst_regno < FIRST_PSEUDO_REGISTER
		  || reg_renumber[dst_regno] >= 0))
	    {
	      for (cand = regno_cands[src_regno];
		   cand != NULL;
		   cand = cand->next_regno_cand)
		if (bitmap_bit_p (avail_cands, cand->index)
		    && bitmap_bit_p (active_cands, cand->index))
		  break;
	    }
	  int i, hard_regno, nregs;
	  int dst_hard_regno, dst_nregs;
	  rtx_insn *remat_insn = NULL;
	  poly_int64 cand_sp_offset = 0;
	  if (cand != NULL)
	    {
	      lra_insn_recog_data_t cand_id
		= lra_get_insn_recog_data (cand->insn);
	      struct lra_static_insn_data *static_cand_id
		= cand_id->insn_static_data;
	      rtx saved_op = *cand_id->operand_loc[cand->nop];

	      /* Check clobbers do not kill something living.  */
	      gcc_assert (REG_P (saved_op));
	      int ignore_regno = REGNO (saved_op); 

	      dst_hard_regno = dst_regno < FIRST_PSEUDO_REGISTER
		? dst_regno : reg_renumber[dst_regno];
	      gcc_assert (dst_hard_regno >= 0);
	      machine_mode mode = GET_MODE (SET_DEST (set));
	      dst_nregs = hard_regno_nregs (dst_hard_regno, mode);

	      for (reg = cand_id->regs; reg != NULL; reg = reg->next)
		if (reg->type != OP_IN && reg->regno != ignore_regno)
		  {
		    hard_regno = get_hard_regs (reg, nregs);
		    gcc_assert (hard_regno >= 0);
		    for (i = 0; i < nregs; i++)
		      if (TEST_HARD_REG_BIT (live_hard_regs, hard_regno + i))
			break;
		    if (i < nregs)
		      break;
		    /* Ensure the clobber also doesn't overlap dst_regno.  */
		    if (hard_regno + nregs > dst_hard_regno
			&& hard_regno < dst_hard_regno + dst_nregs)
		      break;
		  }

	      if (reg == NULL)
		{
		  for (reg = static_cand_id->hard_regs;
		       reg != NULL;
		       reg = reg->next)
		    if (reg->type != OP_IN)
		      {
			if (TEST_HARD_REG_BIT (live_hard_regs, reg->regno))
			  break;
			if (reg->regno >= dst_hard_regno
			    && reg->regno < dst_hard_regno + dst_nregs)
			  break;
		      }
		}

	      if (reg == NULL)
		{
		  *cand_id->operand_loc[cand->nop] = SET_DEST (set);
		  lra_update_insn_regno_info (cand->insn);
		  bool ok_p = lra_constrain_insn (cand->insn);
		  if (ok_p)
		    {
		      rtx remat_pat = copy_insn (PATTERN (cand->insn));
		      
		      start_sequence ();
		      emit_insn (remat_pat);
		      remat_insn = get_insns ();
		      end_sequence ();
		      if (recog_memoized (remat_insn) < 0)
			remat_insn = NULL;
		      cand_sp_offset = cand_id->sp_offset;
		    }
		  *cand_id->operand_loc[cand->nop] = saved_op;
		  lra_update_insn_regno_info (cand->insn);
		}
	    }

	  bitmap_clear (&temp_bitmap);
	  /* Update avail_cands (see analogous code for
	     calculate_gen_cands).  */
	  for (iter = 0; iter < 2; iter++)
	    for (reg = (iter == 0 ? id->regs : static_id->hard_regs);
		 reg != NULL;
		 reg = reg->next)
	      if (reg->type != OP_IN
		  || find_regno_note (insn, REG_DEAD, reg->regno) != NULL)
		EXECUTE_IF_SET_IN_BITMAP (avail_cands, 0, cid, bi)
		  {
		    cand = all_cands[cid];
		    
		    /* Ignore the reload insn.  */
		    if (src_regno == cand->reload_regno
			&& dst_regno == cand->regno)
		      continue;
		    if (cand->regno == reg->regno
			|| reg_overlap_for_remat_p (reg, cand->insn))
		      bitmap_set_bit (&temp_bitmap, cand->index);
		  }

	  if (CALL_P (insn))
	    {
	      function_abi callee_abi = insn_callee_abi (insn);
	      EXECUTE_IF_SET_IN_BITMAP (avail_cands, 0, cid, bi)
		{
		  cand = all_cands[cid];
		
		  if (call_used_input_regno_present_p (callee_abi, cand->insn))
		    bitmap_set_bit (&temp_bitmap, cand->index);
		}
	    }

	  bitmap_and_compl_into (avail_cands, &temp_bitmap);

	  /* Now see whether a candidate is made active or available
	     by this insn.  */
	  cand = insn_to_cand_activation[INSN_UID (insn)];
	  if (cand)
	    bitmap_set_bit (active_cands, cand->index);

	  cand = insn_to_cand[INSN_UID (insn)];
	  if (cand != NULL)
	    {
	      bitmap_set_bit (avail_cands, cand->index);
	      if (cand->reload_regno == -1)
		bitmap_set_bit (active_cands, cand->index);
	      else
		bitmap_clear_bit (active_cands, cand->index);
	    }

	  if (remat_insn != NULL)
	    {
	      poly_int64 sp_offset_change = cand_sp_offset - id->sp_offset;
	      if (maybe_ne (sp_offset_change, 0))
		change_sp_offset (remat_insn, sp_offset_change);
	      update_scratch_ops (remat_insn);
	      lra_process_new_insns (insn, remat_insn, NULL,
				     "Inserting rematerialization insn");
	      lra_set_insn_deleted (insn);
	      changed_p = true;
	      continue;
	    }

	  /* Update live hard regs: */
	  for (reg = id->regs; reg != NULL; reg = reg->next)
	    if (reg->type == OP_IN
		&& find_regno_note (insn, REG_DEAD, reg->regno) != NULL)
	      {
		if ((hard_regno = get_hard_regs (reg, nregs)) < 0)
		  continue;
		for (i = 0; i < nregs; i++)
		  CLEAR_HARD_REG_BIT (live_hard_regs, hard_regno + i);
	      }
	  /* Process also hard regs (e.g. CC register) which are part
	     of insn definition.  */
	  for (reg = static_id->hard_regs; reg != NULL; reg = reg->next)
	    if (reg->type == OP_IN
		&& find_regno_note (insn, REG_DEAD, reg->regno) != NULL)
	      CLEAR_HARD_REG_BIT (live_hard_regs, reg->regno);
	  /* Inputs have been processed, now process outputs.  */
	  for (reg = id->regs; reg != NULL; reg = reg->next)
	    if (reg->type != OP_IN
		&& find_regno_note (insn, REG_UNUSED, reg->regno) == NULL)
	      {
		if ((hard_regno = get_hard_regs (reg, nregs)) < 0)
		  continue;
		for (i = 0; i < nregs; i++)
		  SET_HARD_REG_BIT (live_hard_regs, hard_regno + i);
	      }
	  for (reg = static_id->hard_regs; reg != NULL; reg = reg->next)
	    if (reg->type != OP_IN
		&& find_regno_note (insn, REG_UNUSED, reg->regno) == NULL)
	      SET_HARD_REG_BIT (live_hard_regs, reg->regno);
	}
    }
  return changed_p;
}



/* Current number of rematerialization iteration.  */
int lra_rematerialization_iter;

/* Entry point of the rematerialization sub-pass.  Return true if we
   did any rematerialization.  */
bool
lra_remat (void)
{
  basic_block bb;
  bool result;
  int max_regno = max_reg_num ();

  if (! flag_lra_remat)
    return false;
  lra_rematerialization_iter++;
  if (lra_rematerialization_iter > LRA_MAX_REMATERIALIZATION_PASSES)
    return false;
  if (lra_dump_file != NULL)
    fprintf (lra_dump_file,
	     "\n******** Rematerialization #%d: ********\n\n",
	     lra_rematerialization_iter);
  timevar_push (TV_LRA_REMAT);
  insn_to_cand = XCNEWVEC (cand_t, get_max_uid ());
  insn_to_cand_activation = XCNEWVEC (cand_t, get_max_uid ());
  regno_cands = XCNEWVEC (cand_t, max_regno);
  all_cands.create (8000);
  initiate_cand_table ();
  create_remat_bb_data ();
  bitmap_initialize (&temp_bitmap, &reg_obstack);
  bitmap_initialize (&subreg_regs, &reg_obstack);
  calculate_local_reg_remat_bb_data ();
  create_cands ();
  calculate_livein_cands ();
  calculate_gen_cands ();
  bitmap_initialize (&all_blocks, &reg_obstack);
  FOR_ALL_BB_FN (bb, cfun)
    bitmap_set_bit (&all_blocks, bb->index);
  calculate_global_remat_bb_data ();
  dump_candidates_and_remat_bb_data ();
  result = do_remat ();
  all_cands.release ();
  bitmap_clear (&temp_bitmap);
  bitmap_clear (&subreg_regs);
  finish_remat_bb_data ();
  finish_cand_table ();
  bitmap_clear (&all_blocks);
  free (regno_cands);
  free (insn_to_cand);
  free (insn_to_cand_activation);
  timevar_pop (TV_LRA_REMAT);
  return result;
}
