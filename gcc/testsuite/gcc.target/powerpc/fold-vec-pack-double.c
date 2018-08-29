/* Verify that overloaded built-ins for vec_pack with
   double inputs produce the right results.  */

/* { dg-do compile } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power8" } } */
/* { dg-options "-mvsx -mpower8-vector -O2" } */

#include <altivec.h>

// vector float vec_pack (vector double, vector double);

vector float
test_pack (vector double vd2, vector double vd3)
{
  return vec_pack (vd2, vd3);
}

/* { dg-final { scan-assembler-times "vmrgew" 1 { target be } } } */
/* { dg-final { scan-assembler-times "vmrgow"  1 { target le } } } */
