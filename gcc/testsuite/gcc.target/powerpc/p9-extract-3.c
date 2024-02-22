/* { dg-do compile { target lp64 } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mdejagnu-cpu=power9 -mvsx -O2" } */

/* Test that under ISA 3.0 (-mcpu=power9), the compiler optimizes conversion to
   double after a vec_extract to use the VEXTRACTU{B,H} or XXEXTRACTUW
   instructions (which leaves the result in a vector register), and not the
   VEXTU{B,H,W}{L,R}X instructions (which needs a direct move to do the floating
   point conversion).  */

#include <altivec.h>

double
fpcvt_int_0 (vector int a)
{
  int c = 0;
  int b = vec_extract (a, c);
  return (double)b;
}

double
fpcvt_int_3 (vector int a)
{
  int c = 3;
  int b = vec_extract (a, c);
  return (double)b;
}

double
fpcvt_uint_0 (vector unsigned int a)
{
  int c = 0;
  unsigned int b = vec_extract (a, c);
  return (double)b;
}

double
fpcvt_uint_3 (vector unsigned int a)
{
  int c = 3;
  unsigned int b = vec_extract (a, c);
  return (double)b;
}

double
fpcvt_short_0 (vector short a)
{
  int c = 0;
  short b = vec_extract (a, c);
  return (double)b;
}

double
fpcvt_short_7 (vector short a)
{
  int c = 7;
  short b = vec_extract (a, c);
  return (double)b;
}

double
fpcvt_ushort_0 (vector unsigned short a)
{
  int c = 0;
  unsigned short b = vec_extract (a, c);
  return (double)b;
}

double
fpcvt_ushort_7 (vector unsigned short a)
{
  int c = 7;
  unsigned short b = vec_extract (a, c);
  return (double)b;
}

double
fpcvt_schar_0 (vector signed char a)
{
  int c = 0;
  signed char b = vec_extract (a, c);
  return (double)b;
}

double
fpcvt_schar_15 (vector signed char a)
{
  int c = 15;
  signed char b = vec_extract (a, c);
  return (double)b;
}

double
fpcvt_uchar_0 (vector unsigned char a)
{
  int c = 0;
  unsigned char b = vec_extract (a, c);
  return (double)b;
}

double
fpcvt_uchar_15 (vector unsigned char a)
{
  int c = 15;
  signed char b = vec_extract (a, c);
  return (double)b;
}

/* { dg-final { scan-assembler     "vextractu\[bh\] "    } } */
/* { dg-final { scan-assembler     "vexts\[bh\]2d "      } } */
/* { dg-final { scan-assembler     "vspltw "             } } */
/* { dg-final { scan-assembler     "xscvsxddp "          } } */
/* { dg-final { scan-assembler     "xvcvsxwdp "          } } */
/* { dg-final { scan-assembler     "xvcvuxwdp "          } } */
/* { dg-final { scan-assembler-not "exts\[bhw\] "        } } */
/* { dg-final { scan-assembler-not "stxv"                } } */
/* { dg-final { scan-assembler-not "m\[ft\]vsrd "        } } */
/* { dg-final { scan-assembler-not "m\[ft\]vsrw\[az\] "  } } */
/* { dg-final { scan-assembler-not "l\[hw\]\[az\] "      } } */
