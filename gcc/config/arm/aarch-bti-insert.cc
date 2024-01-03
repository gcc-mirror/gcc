/* Branch Target Identification for AArch64 architecture.
   Copyright (C) 2019-2024 Free Software Foundation, Inc.
   Contributed by Arm Ltd.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#define IN_TARGET_CODE 1

#include "config.h"
#define INCLUDE_STRING
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "tree.h"
#include "memmodel.h"
#include "gimple.h"
#include "tm_p.h"
#include "stringpool.h"
#include "attribs.h"
#include "emit-rtl.h"
#include "gimplify.h"
#include "gimple-iterator.h"
#include "dumpfile.h"
#include "rtl-iter.h"
#include "cfgrtl.h"
#include "tree-pass.h"
#include "cgraph.h"

/* This pass enables the support for Branch Target Identification Mechanism for
   Arm/AArch64.  This is a security feature introduced in ARMv8.5-A
   architecture and ARMv8.1-M.  A BTI instruction is used to guard against the
   execution of instructions which are not the intended target of an indirect
   branch.

   Outside of a guarded memory region, a BTI instruction executes as a NOP.
   Within a guarded memory region any target of an indirect branch must be
   a compatible BTI or BRK, HLT, PACIASP, PACIBASP instruction (even if the
   branch is triggered in a non-guarded memory region).  An incompatibility
   generates a Branch Target Exception.

   The compatibility of the BTI instruction is as follows (AArch64
   examples):
   BTI j : Can be a target of any indirect jump (BR Xn).
   BTI c : Can be a target of any indirect call (BLR Xn and BR X16/X17).
   BTI jc: Can be a target of any indirect call or indirect jump.
   BTI   : Can not be a target of any indirect call or indirect jump.

  In order to enable this mechanism, this pass iterates through the
  control flow of the code and adds appropriate BTI instructions :
  * Add a new "BTI C" at the beginning of a function, unless its already
    protected by a "PACIASP/PACIBSP".  We exempt the functions that are only
    called directly.
  * Add a new "BTI J" for every target of an indirect jump, jump table targets,
    non-local goto targets or labels that might be referenced by variables,
    constant pools, etc (NOTE_INSN_DELETED_LABEL)

  Since we have already changed the use of indirect tail calls to only x16
  and x17, we do not have to use "BTI JC".

  This pass is triggered by the command line option -mbranch-protection=bti or
  -mbranch-protection=standard.  Since all the BTI instructions are in the HINT
  space, this pass does not require any minimum architecture version.  */

namespace {

const pass_data pass_data_insert_bti =
{
  RTL_PASS, /* type.  */
  "bti", /* name.  */
  OPTGROUP_NONE, /* optinfo_flags.  */
  TV_MACH_DEP, /* tv_id.  */
  0, /* properties_required.  */
  0, /* properties_provided.  */
  0, /* properties_destroyed.  */
  0, /* todo_flags_start.  */
  0, /* todo_flags_finish.  */
};

/* Insert the BTI instruction.  */
/* This is implemented as a late RTL pass that runs before branch
   shortening and does the following.  */
static unsigned int
rest_of_insert_bti (void)
{
  timevar_push (TV_MACH_DEP);

  rtx bti_insn;
  rtx_insn *insn;
  basic_block bb;

  bb = 0;
  FOR_EACH_BB_FN (bb, cfun)
    {
      for (insn = BB_HEAD (bb); insn != NEXT_INSN (BB_END (bb));
	   insn = NEXT_INSN (insn))
	{
	  /* If a label is marked to be preserved or can be a non-local goto
	     target, it must be protected with a BTI J.  */
	  if (LABEL_P (insn)
	       && (LABEL_PRESERVE_P (insn)
		   || bb->flags & BB_NON_LOCAL_GOTO_TARGET))
	    {
	      bti_insn = aarch_gen_bti_j ();
	      emit_insn_after (bti_insn, insn);
	      continue;
	    }

	  /* There could still be more labels that are valid targets of a
	     BTI J instuction.  To find them we start looking through the
	     JUMP_INSN.  If it jumps to a jump table, then we find all labels
	     of the jump table to protect with a BTI J.  */
	  if (JUMP_P (insn))
	    {
	      rtx_jump_table_data *table;
	      if (tablejump_p (insn, NULL, &table))
		{
		  rtvec vec = table->get_labels ();
		  int j;
		  rtx_insn *label;

		  for (j = GET_NUM_ELEM (vec) - 1; j >= 0; --j)
		    {
		      label = as_a <rtx_insn *> (XEXP (RTVEC_ELT (vec, j), 0));
		      rtx_insn *next = next_nonnote_nondebug_insn (label);
		      if (aarch_bti_j_insn_p (next))
			continue;

		      bti_insn = aarch_gen_bti_j ();
		      emit_insn_after (bti_insn, label);
		    }
		}
	    }

	  /* Also look for calls to setjmp () which would be marked with
	     REG_SETJMP note and put a BTI J after.  This is where longjump ()
	     will return.  */
	  if (CALL_P (insn) && (find_reg_note (insn, REG_SETJMP, NULL)))
	    {
	      bti_insn = aarch_gen_bti_j ();
	      emit_insn_after (bti_insn, insn);
	      continue;
	    }
	}
    }

  /* Since a Branch Target Exception can only be triggered by an indirect call,
     we exempt function that are only called directly.  We also exempt
     functions that are already protected by Return Address Signing (PACIASP/
     PACIBSP).  For all other cases insert a BTI C at the beginning of the
     function.  */
  if (!cgraph_node::get (cfun->decl)->only_called_directly_p ())
    {
      bb = ENTRY_BLOCK_PTR_FOR_FN (cfun)->next_bb;
      insn = BB_HEAD (bb);
      if (!aarch_pac_insn_p (get_first_nonnote_insn ()))
	{
	  bti_insn = aarch_gen_bti_c ();
	  emit_insn_before (bti_insn, insn);
	}
    }

  timevar_pop (TV_MACH_DEP);
  return 0;
}


class pass_insert_bti : public rtl_opt_pass
{
public:
  pass_insert_bti (gcc::context *ctxt)
    : rtl_opt_pass (pass_data_insert_bti, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *)
    {
      if (aarch_bti_enabled ())
        {
          aarch_bti_arch_check ();
          return true;
        }
      return false;
    }

  virtual unsigned int execute (function *)
    {
      return rest_of_insert_bti ();
    }

}; // class pass_insert_bti

} // anon namespace

rtl_opt_pass *
make_pass_insert_bti (gcc::context *ctxt)
{
  return new pass_insert_bti (ctxt);
}
