/* { dg-do compile } */
/* { dg-options "-O3" } */

#include <arm_sve.h>

 void
two_scans (unsigned char *p, unsigned char *q, unsigned long *r1,
           unsigned long *r2, unsigned char *o1, unsigned char *o2)
{
  svbool_t pt = svptrue_b8 ();
  svsetffr ();
  svuint8_t a = svldff1_u8 (pt, p);
  unsigned long n1 = svcntp_b8 (pt, svrdffr ());
  svsetffr ();
  svuint8_t b = svldff1_u8 (pt, q);
  unsigned long n2 = svcntp_b8 (pt, svrdffr ());
  svst1_u8 (pt, o1, a);
  svst1_u8 (pt, o2, b);
  *r1 = n1;
  *r2 = n2;
}

/* { dg-final { scan-assembler-times {\trdffr} 2 } } */
