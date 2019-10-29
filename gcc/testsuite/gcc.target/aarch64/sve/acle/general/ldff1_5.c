/* { dg-do compile } */
/* { dg-options "-O2" } */

#include <arm_sve.h>

/* Make sure that we can use RDFFRS to read the FFR while testing for a
   fault.  */
svint8_t
foo (svbool_t pg, int8_t *ptr, svbool_t *pred, int *fault)
{
  svsetffr ();
  svint8_t x = svldff1 (pg, ptr);
  svbool_t ffr = svrdffr_z (pg);
  *fault = svptest_any (pg, ffr);
  *pred = ffr;
  return x;
}

/* { dg-final { scan-assembler {\tsetffr\n.*\tldff1b\t.*\trdffrs\t} } } */
/* { dg-final { scan-assembler-not {\trdffr\t} } } */
