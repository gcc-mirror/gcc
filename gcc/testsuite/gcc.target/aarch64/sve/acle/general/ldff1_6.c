/* { dg-do compile } */
/* { dg-options "-O2" } */

#include <arm_sve.h>

/* Make sure that we can use RDFFRS to test for a fault.  */
svint8_t
foo (svbool_t pg, int8_t *ptr, int *fault)
{
  svsetffr ();
  svint8_t x = svldff1 (pg, ptr);
  *fault = svptest_any (svptrue_b8 (), svrdffr ());
  return x;
}

/* { dg-final { scan-assembler {\tsetffr\n.*\tldff1b\t.*\trdffrs\t} } } */
/* { dg-final { scan-assembler-not {\trdffr\t} } } */
