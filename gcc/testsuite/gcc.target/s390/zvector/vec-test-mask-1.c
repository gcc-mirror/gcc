/* { dg-do compile { target { s390*-*-* } } } */
/* { dg-options "-O3 -mzarch -march=z13 -mzvector" } */

/* { dg-final { scan-assembler-times "vtm" 2 } } */
/* { dg-final { scan-assembler-times "ipm" 1 } } */

#include <vecintrin.h>

/* CC will be extracted into a GPR and returned.  */
int
foo (vector unsigned int a, vector unsigned b)
{
  return vec_test_mask (a, b);
}

extern void baz (void);

/* In that case the ipm/srl is supposed to optimized out by
   combine/s390_canonicalize_comparison.  */
int
bar (vector unsigned int a, vector unsigned b)
{
  if (vec_test_mask (a, b) == 2)
    baz ();
}
