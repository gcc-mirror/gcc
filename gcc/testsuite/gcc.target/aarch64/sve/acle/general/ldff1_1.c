/* { dg-do compile } */
/* { dg-options "-O2" } */

#include <arm_sve.h>

/* Make sure that SETFFR comes first, however high the priority of the
   LDFF1 is.  */
svint8_t
foo (svbool_t pg, int8_t *ptr)
{
  svsetffr ();
  svint8_t x = svldff1 (pg, ptr);
  x = svadd_x (pg, x, x);
  x = svmul_x (pg, x, x);
  return x;
}

/* { dg-final { scan-assembler {\tsetffr\n.*\tldff1b\t} } } */
