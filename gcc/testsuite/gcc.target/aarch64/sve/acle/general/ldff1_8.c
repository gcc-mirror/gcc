/* { dg-do compile } */
/* { dg-options "-O2" } */

#include <arm_sve.h>

void
foo (int8_t *ptr1, int8_t *ptr2, svint8_t *res1, svint8_t *res2)
{
  svbool_t pg = svptrue_b8 ();

  svsetffr ();
  svint8_t x1 = svldff1 (pg, ptr1);
  svbool_t ok1 = svrdffr ();
  if (!svptest_last (pg, ok1))
    {
      x1 = svsel (ok1, x1, svdup_s8 (0));
      svsetffr ();
    }

  svint8_t x2 = svldff1 (pg, ptr2);
  svbool_t ok2 = svrdffr ();
  if (!svptest_last (pg, ok2))
    x2 = svsel (ok2, x2, svdup_s8 (0));

  *res1 = x1;
  *res2 = x2;
}

/* { dg-final { scan-assembler-times {\trdffrs\t} 2 } } */
/* { dg-final { scan-assembler-times {\t(?:b.last|b.nfrst)\t} 2 } } */
/* { dg-final { scan-assembler-not {\trdffr\t} } } */
/* { dg-final { scan-assembler-not {\tptest\t} } } */
