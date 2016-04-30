/* Check that the div0s instruction is used for integer sign comparisons
   when -mpretend-cmove is enabled.
   Each test case is expected to emit at least one div0s insn.
   Problems when combining the div0s comparison result with surrounding
   logic usually show up as redundant tst insns.  */
/* { dg-do compile }  */
/* { dg-options "-O2 -mpretend-cmove" } */
/* { dg-final { scan-assembler-times "div0s" 32 } } */
/* { dg-final { scan-assembler-not "tst" } } */
/* { dg-final { scan-assembler-not "not\t" } }  */
/* { dg-final { scan-assembler-not "nott" } }  */

/* { dg-final { scan-assembler-times "negc" 9 { target { ! sh2a } } } }  */
/* { dg-final { scan-assembler-times "movrt" 9 { target { sh2a } } } }  */

#include "pr52933-1.c"
