/* Check that the div0s instruction is used for integer sign comparisons
   when -mpretend-cmove is enabled.
   Each test case is expected to emit at least one div0s insn.
   Problems when combining the div0s comparison result with surrounding
   logic usually show up as redundant tst insns.  */
/* { dg-do compile }  */
/* { dg-options "-O2 -mpretend-cmove" } */
/* { dg-skip-if "" { "sh*-*-*" } { "-m5*" } { "" } } */
/* { dg-final { scan-assembler-times "div0s" 25 } } */
/* { dg-final { scan-assembler-not "tst" } } */

#include "pr52933-1.c"
