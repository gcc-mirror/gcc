/* { dg-do compile } */
/* { dg-options "-O3" } */

#include <arm_sve.h>

 void
two_scans (svuint32_t p, svuint32_t q, unsigned long *r1,
           unsigned long *r2, unsigned *o1, unsigned *o2,
           long int offset1, long int offset2)
{
  svbool_t pt = svptrue_b8 ();
  svsetffr ();
  svuint32_t a = svldff1sb_gather_u32base_offset_u32 (pt, p, offset1);
  unsigned long n1 = svcntp_b8 (pt, svrdffr ());
  svsetffr ();
  svuint32_t b = svldff1sb_gather_u32base_offset_u32 (pt, q, offset2);
  unsigned long n2 = svcntp_b8 (pt, svrdffr ());
  svst1_u32 (pt, o1, a);
  svst1_u32 (pt, o2, b);
  *r1 = n1;
  *r2 = n2;
}

/* { dg-final { scan-assembler-times {\trdffr} 2 } } */
