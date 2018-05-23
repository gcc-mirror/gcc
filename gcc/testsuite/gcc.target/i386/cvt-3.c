/* { dg-do compile } */
/* { dg-options "-O2 -mavx512f -mfpmath=sse" } */

float ui2f (unsigned int x) { return x; }
double ui2d (unsigned int x) { return x; }

#ifdef __x86_64__
float ul2f (unsigned long x) { return x; }
double ul2d (unsigned long x) { return x; }
#endif
  
/* { dg-final { scan-assembler-times "vcvtusi2ss" 1 { target ia32 } } } */
/* { dg-final { scan-assembler-times "vcvtusi2sd" 1 { target ia32 } } } */
/* { dg-final { scan-assembler-times "vcvtusi2ss" 2 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "vcvtusi2sd" 2 { target { ! ia32 } } } } */
