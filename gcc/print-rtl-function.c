/* Print RTL functions for GCC.
   Copyright (C) 2016 Free Software Foundation, Inc.

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
#include "tm.h"
#include "rtl.h"
#include "alias.h"
#include "tree.h"
#include "cfg.h"
#include "flags.h"
#include "predict.h"
#include "function.h"
#include "basic-block.h"
#include "print-rtl.h"
#include "langhooks.h"
#include "memmodel.h"
#include "emit-rtl.h"

extern bool flag_compact;

/* Print an "(edge-from)" or "(edge-to)" directive describing E
   to OUTFILE.  */

static void
print_edge (FILE *outfile, edge e, bool from)
{
  fprintf (outfile, "      (%s ", from ? "edge-from" : "edge-to");
  basic_block bb = from ? e->src : e->dest;
  gcc_assert (bb);
  switch (bb->index)
    {
    case ENTRY_BLOCK:
      fprintf (outfile, "entry");
      break;
    case EXIT_BLOCK:
      fprintf (outfile, "exit");
      break;
    default:
      fprintf (outfile, "%i", bb->index);
      break;
    }

  /* Express edge flags as a string with " | " separator.
     e.g. (flags "FALLTHRU | DFS_BACK").  */
  fprintf (outfile, " (flags \"");
  bool seen_flag = false;
#define DEF_EDGE_FLAG(NAME,IDX) \
  do {						\
    if (e->flags & EDGE_##NAME)			\
      {						\
	if (seen_flag)				\
	  fprintf (outfile, " | ");		\
	fprintf (outfile, "%s", (#NAME));	\
	seen_flag = true;			\
      }						\
  } while (0);
#include "cfg-flags.def"
#undef DEF_EDGE_FLAG

  fprintf (outfile, "\"))\n");
}

/* If BB is non-NULL, print the start of a "(block)" directive for it
   to OUTFILE, otherwise do nothing.  */

static void
begin_any_block (FILE *outfile, basic_block bb)
{
  if (!bb)
    return;

  edge e;
  edge_iterator ei;

  fprintf (outfile, "    (block %i\n", bb->index);
  FOR_EACH_EDGE (e, ei, bb->preds)
    print_edge (outfile, e, true);
}

/* If BB is non-NULL, print the end of a "(block)" directive for it
   to OUTFILE, otherwise do nothing.  */

static void
end_any_block (FILE *outfile, basic_block bb)
{
  if (!bb)
    return;

  edge e;
  edge_iterator ei;

  FOR_EACH_EDGE (e, ei, bb->succs)
    print_edge (outfile, e, false);
  fprintf (outfile, "    ) ;; block %i\n", bb->index);
}

/* Determine if INSN is of a kind that can have a basic block.  */

static bool
can_have_basic_block_p (const rtx_insn *insn)
{
  rtx_code code = GET_CODE (insn);
  if (code == BARRIER)
    return false;
  gcc_assert (GET_RTX_FORMAT (code)[2] == 'B');
  return true;
}

/* Write FN to OUTFILE in a form suitable for parsing, with indentation
   and comments to make the structure easy for a human to grok.  Track
   the basic blocks of insns in the chain, wrapping those that are within
   blocks within "(block)" directives.

   If COMPACT, then instructions are printed in a compact form:
   - INSN_UIDs are omitted, except for jumps and CODE_LABELs,
   - INSN_CODEs are omitted,
   - register numbers are omitted for hard and virtual regs
   - insn names are prefixed with "c" (e.g. "cinsn", "cnote", etc)

   Example output (with COMPACT==true):

   (function "times_two"
     (insn-chain
       (cnote NOTE_INSN_DELETED)
       (block 2
	 (edge-from entry (flags "FALLTHRU"))
	 (cnote [bb 2] NOTE_INSN_BASIC_BLOCK)
	 (cinsn (set (mem/c:SI (plus:DI (reg/f:DI virtual-stack-vars)
			       (const_int -4)) [1 i+0 S4 A32])
		       (reg:SI di [ i ])) "t.c":2
		   (nil))
	 (cnote NOTE_INSN_FUNCTION_BEG)
	 (cinsn (set (reg:SI 89)
		       (mem/c:SI (plus:DI (reg/f:DI virtual-stack-vars)
			       (const_int -4)) [1 i+0 S4 A32])) "t.c":3
		   (nil))
	 (cinsn (parallel [
			   (set (reg:SI 87 [ _2 ])
			       (ashift:SI (reg:SI 89)
				   (const_int 1)))
			   (clobber (reg:CC flags))
		       ]) "t.c":3
		   (expr_list:REG_EQUAL (ashift:SI (mem/c:SI (plus:DI (reg/f:DI virtual-stack-vars)
				   (const_int -4)) [1 i+0 S4 A32])
			   (const_int 1))
		       (nil)))
	 (cinsn (set (reg:SI 88 [ <retval> ])
		       (reg:SI 87 [ _2 ])) "t.c":3
		   (nil))
	 (cinsn (set (reg/i:SI ax)
		       (reg:SI 88 [ <retval> ])) "t.c":4
		   (nil))
	 (cinsn (use (reg/i:SI ax)) "t.c":4
		   (nil))
	 (edge-to exit (flags "FALLTHRU"))
       ) ;; block 2
     ) ;; insn-chain
     (crtl
       (return_rtx
	 (reg/i:SI ax)
       ) ;; return_rtx
     ) ;; crtl
   ) ;; function "times_two"
*/

DEBUG_FUNCTION void
print_rtx_function (FILE *outfile, function *fn, bool compact)
{
  flag_compact = compact;

  tree fdecl = fn->decl;

  const char *dname = lang_hooks.decl_printable_name (fdecl, 2);

  fprintf (outfile, "(function \"%s\"\n", dname);

  /* The instruction chain.  */
  fprintf (outfile, "  (insn-chain\n");
  basic_block curr_bb = NULL;
  for (rtx_insn *insn = get_insns (); insn; insn = NEXT_INSN (insn))
    {
      basic_block insn_bb;
      if (can_have_basic_block_p (insn))
	insn_bb = BLOCK_FOR_INSN (insn);
      else
	insn_bb = NULL;
      if (curr_bb != insn_bb)
	{
	  end_any_block (outfile, curr_bb);
	  curr_bb = insn_bb;
	  begin_any_block (outfile, curr_bb);
	}
      print_rtl_single_with_indent (outfile, insn, curr_bb ? 6 : 4);
    }
  end_any_block (outfile, curr_bb);
  fprintf (outfile, "  ) ;; insn-chain\n");

  /* Additional RTL state.  */
  fprintf (outfile, "  (crtl\n");
  fprintf (outfile, "    (return_rtx \n");
  print_rtl_single_with_indent (outfile, crtl->return_rtx, 6);
  fprintf (outfile, "    ) ;; return_rtx\n");
  fprintf (outfile, "  ) ;; crtl\n");

  fprintf (outfile, ") ;; function \"%s\"\n", dname);

  flag_compact = false;
}
