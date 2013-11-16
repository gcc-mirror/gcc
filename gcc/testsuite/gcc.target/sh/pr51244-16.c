/* Check that the redundant test removal code in the *cbranch_t split works
   as expected on SH2A targets.  */
/* { dg-do compile }  */
/* { dg-options "-O2" } */
/* { dg-skip-if "" { "sh*-*-*" } { "*" } { "-m2a*" } } */
/* { dg-final { scan-assembler-times "tst" 6 } } */
/* { dg-final { scan-assembler-times "movt" 3 } } */
/* { dg-final { scan-assembler-times "movrt" 3 } } */
/* { dg-final { scan-assembler-not "extu|exts|negc" } } */

#include "pr51244-15.c"
