/* { dg-do compile } */ 
/* { dg-options "-O2 -fdump-tree-vrp1-details -fdump-tree-dom2-details -std=gnu89 --param logical-op-non-short-circuit=0" } */

#include "ssa-dom-thread-4.c"

/* On targets that define LOGICAL_OP_NON_SHORT_CIRCUIT to 0, we split both
   "a_elt || b_elt" and "b_elt && kill_elt" into two conditions each,
   rather than using "(var1 != 0) op (var2 != 0)".  Also, as on other targets,
   we duplicate the header of the inner "while" loop.  There are then
   4 threading opportunities:

   1x "!a_elt && b_elt" in the outer "while" loop
      -> the start of the inner "while" loop,
	 skipping the known-true "b_elt" in the first condition.
   1x "!b_elt" in the first condition
      -> the outer "while" loop's continuation point,
	 skipping the known-false "b_elt" in the second condition.
   2x "kill_elt->indx >= b_elt->indx" in the first "while" loop
      -> "kill_elt->indx == b_elt->indx" in the second condition,
	 skipping the known-true "b_elt && kill_elt" in the second
	 condition.

   All the cases are picked up by VRP1 as jump threads.  */
/* { dg-final { scan-tree-dump-times "Threaded" 4 "vrp1" } } */
