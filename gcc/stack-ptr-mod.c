/* Discover if the stack pointer is modified in a function.
   Copyright (C) 2007-2019 Free Software Foundation, Inc.

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
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "rtl.h"
#include "df.h"
#include "memmodel.h"
#include "emit-rtl.h"
#include "tree-pass.h"

/* Determine if the stack pointer is constant over the life of the function.
   Only useful before prologues have been emitted.  */

static void
notice_stack_pointer_modification_1 (rtx x, const_rtx pat ATTRIBUTE_UNUSED,
				     void *data ATTRIBUTE_UNUSED)
{
  if (x == stack_pointer_rtx
      /* The stack pointer is only modified indirectly as the result
	 of a push until later.  See the comments in rtl.texi
	 regarding Embedded Side-Effects on Addresses.  */
      || (MEM_P (x)
	  && GET_RTX_CLASS (GET_CODE (XEXP (x, 0))) == RTX_AUTOINC
	  && XEXP (XEXP (x, 0), 0) == stack_pointer_rtx))
    crtl->sp_is_unchanging = 0;
}

  /* Some targets can emit simpler epilogues if they know that sp was
     not ever modified during the function.  After reload, of course,
     we've already emitted the epilogue so there's no sense searching.  */

namespace {

const pass_data pass_data_stack_ptr_mod =
{
  RTL_PASS, /* type */
  "*stack_ptr_mod", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_NONE, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_stack_ptr_mod : public rtl_opt_pass
{
public:
  pass_stack_ptr_mod (gcc::context *ctxt)
    : rtl_opt_pass (pass_data_stack_ptr_mod, ctxt)
  {}

  /* opt_pass methods: */
  virtual unsigned int execute (function *);

}; // class pass_stack_ptr_mod

unsigned int
pass_stack_ptr_mod::execute (function *fun)
{
  basic_block bb;
  rtx_insn *insn;

  /* Assume that the stack pointer is unchanging if alloca hasn't
     been used.  */
  crtl->sp_is_unchanging = !fun->calls_alloca;
  if (crtl->sp_is_unchanging)
    FOR_EACH_BB_FN (bb, fun)
      FOR_BB_INSNS (bb, insn)
        {
	  if (INSN_P (insn))
	    {
	      /* Check if insn modifies the stack pointer.  */
	      note_stores (insn, notice_stack_pointer_modification_1, NULL);
	      if (! crtl->sp_is_unchanging)
		return 0;
	    }
	}

  /* The value coming into this pass was 0, and the exit block uses
     are based on this.  If the value is now 1, we need to redo the
     exit block uses.  */
  if (df && crtl->sp_is_unchanging)
    df_update_exit_block_uses ();

  return 0;
}

} // anon namespace

rtl_opt_pass *
make_pass_stack_ptr_mod (gcc::context *ctxt)
{
  return new pass_stack_ptr_mod (ctxt);
}
