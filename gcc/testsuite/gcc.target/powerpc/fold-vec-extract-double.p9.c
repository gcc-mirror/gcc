/* Verify that overloaded built-ins for vec_extract() with
   double inputs produce the right code.  */

/* { dg-do compile { target { p9vector_hw } } } */
/* { dg-require-effective-target p9vector_hw } */
/* { dg-options "-mdejagnu-cpu=power9 -O2 " } */

/* { dg-final { scan-assembler-times {\mxxlor\M} 2 { target lp64} } } */
/* { dg-final { scan-assembler-times {\mrldic\M} 1 { target lp64} } } */
/* { dg-final { scan-assembler-times {\mmtvsrdd\M} 1 { target lp64} } } */
/* { dg-final { scan-assembler-times {\mvslo\M} 1 { target lp64} } } */

#include <altivec.h>

double
testd_var (vector double vd2, signed int si)
{
  return vec_extract (vd2, si);
}

double
testd_cst (vector double vd2)
{
  return vec_extract (vd2, 1);
}

