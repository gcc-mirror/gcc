/* { dg-do compile } */
/* { dg-options "-O2 -fschedule-insns" } */

#include <arm_sve.h>

/* Make sure that LDFF1s can be reordered.  The load of x should come due
   to its longer dependence chain.  */
svint8_t
foo (int8_t *ptr1, int8_t *ptr2)
{
  svsetffr ();
  svbool_t pg = svptrue_b8 ();
  svint8_t y = svldff1 (pg, ptr2);
  svint8_t x = svldff1 (pg, ptr1);
  x = svadd_x (pg, x, x);
  x = svmul_x (pg, x, x);
  x = svadd_x (pg, x, y);
  return x;
}

/* { dg-final { scan-assembler {\tldff1b\tz[0-9]+\.b, p[0-7]/z, \[x0\]\n.*\tldff1b\tz[0-9]+\.b, p[0-7]/z, \[x1\]\n} } } */
