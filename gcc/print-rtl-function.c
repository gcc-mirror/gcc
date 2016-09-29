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
#include "emit-rtl.h"

/* Write FN to OUTFILE in a form suitable for parsing, with indentation
   and comments to make the structure easy for a human to grok.

   Example output:

     (function "times_two"
       (insn-chain
	 (note 1 0 4 (nil) NOTE_INSN_DELETED)
	 (note 4 1 2 2 [bb 2] NOTE_INSN_BASIC_BLOCK)
	 (insn 2 4 3 2 (set (mem/c:SI (plus:DI (reg/f:DI 82 virtual-stack-vars)
			     (const_int -4 [0xfffffffffffffffc])) [1 i+0 S4 A32])
		     (reg:SI 5 di [ i ])) t.c:2 -1
		  (nil))
	 (note 3 2 6 2 NOTE_INSN_FUNCTION_BEG)
	 (insn 6 3 7 2 (set (reg:SI 89)
		     (mem/c:SI (plus:DI (reg/f:DI 82 virtual-stack-vars)
			     (const_int -4 [0xfffffffffffffffc])) [1 i+0 S4 A32])) t.c:3 -1
		  (nil))
	 (insn 7 6 10 2 (parallel [
			 (set (reg:SI 87 [ _2 ])
			     (ashift:SI (reg:SI 89)
				 (const_int 1 [0x1])))
			 (clobber (reg:CC 17 flags))
		     ]) t.c:3 -1
		  (expr_list:REG_EQUAL (ashift:SI (mem/c:SI (plus:DI (reg/f:DI 82 virtual-stack-vars)
				 (const_int -4 [0xfffffffffffffffc])) [1 i+0 S4 A32])
			 (const_int 1 [0x1]))
		     (nil)))
	 (insn 10 7 14 2 (set (reg:SI 88 [ <retval> ])
		     (reg:SI 87 [ _2 ])) t.c:3 -1
		  (nil))
	 (insn 14 10 15 2 (set (reg/i:SI 0 ax)
		     (reg:SI 88 [ <retval> ])) t.c:4 -1
		  (nil))
	 (insn 15 14 0 2 (use (reg/i:SI 0 ax)) t.c:4 -1
		  (nil))
       ) ;; insn-chain
       (cfg
	 (bb 0
	   (edge 0 2 (flags 0x1))
	 ) ;; bb
	 (bb 2
	   (edge 2 1 (flags 0x1))
	 ) ;; bb
	 (bb 1
	 ) ;; bb
       ) ;; cfg
       (crtl
	 (return_rtx
	   (reg/i:SI 0 ax)
	 ) ;; return_rtx
       ) ;; crtl
     ) ;; function "times_two"
*/

DEBUG_FUNCTION void
print_rtx_function (FILE *outfile, function *fn)
{
  tree fdecl = fn->decl;

  const char *dname = lang_hooks.decl_printable_name (fdecl, 2);

  fprintf (outfile, "(function \"%s\"\n", dname);

  /* The instruction chain.  */
  fprintf (outfile, "  (insn-chain\n");
  for (rtx_insn *insn = get_insns (); insn; insn = NEXT_INSN (insn))
    print_rtl_single_with_indent (outfile, insn, 4);
  fprintf (outfile, "  ) ;; insn-chain\n");

  /* The CFG.  */
  fprintf (outfile, "  (cfg\n");
  {
    basic_block bb;
    FOR_ALL_BB_FN (bb, fn)
      {
	fprintf (outfile, "    (bb %i\n", bb->index);
	edge e;
	edge_iterator ei;
	FOR_EACH_EDGE (e, ei, bb->succs)
	  fprintf (outfile, "      (edge %i %i (flags 0x%x))\n",
		   e->src->index, e->dest->index, e->flags);
	fprintf (outfile, "    ) ;; bb\n");
      }
  }
  fprintf (outfile, "  ) ;; cfg\n");

  /* Additional RTL state.  */
  fprintf (outfile, "  (crtl\n");
  fprintf (outfile, "    (return_rtx \n");
  print_rtl_single_with_indent (outfile, crtl->return_rtx, 6);
  fprintf (outfile, "    ) ;; return_rtx\n");
  fprintf (outfile, "  ) ;; crtl\n");

  fprintf (outfile, ") ;; function \"%s\"\n", dname);
}
