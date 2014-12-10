/* Check that the SH specific sh_treg_combine RTL optimization pass works as
   expected.  */
/* { dg-do compile }  */
/* { dg-options "-O2" } */
/* { dg-skip-if "" { "sh*-*-*" } { "*" } { "-m2a*" } } */
/* { dg-final { scan-assembler-times "tst" 6 } } */
/* { dg-final { scan-assembler-times "movt" 1 } } */
/* { dg-final { scan-assembler-times "nott" 1 } } */
/* { dg-final { scan-assembler-times "cmp/eq" 2 } } */
/* { dg-final { scan-assembler-times "cmp/hi" 4 } } */
/* { dg-final { scan-assembler-times "cmp/gt" 3 } } */
/* { dg-final { scan-assembler-not "not\t" } } */

#include "pr51244-20.c"
