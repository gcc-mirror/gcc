/* { dg-do compile } */
/* { dg-options "-O2 -mavx512f -mfpmath=sse" } */

unsigned int f2ui (float x) { return x; }
unsigned int d2ui (double x) { return x; }

#ifdef __x86_64__
unsigned long f2ul (float x) { return x; }
unsigned long d2ul (double x) { return x; }
#endif
  
/* { dg-final { scan-assembler-times "vcvttss2usi" 1 { target ia32 } } } */
/* { dg-final { scan-assembler-times "vcvttsd2usi" 1 { target ia32 } } } */
/* { dg-final { scan-assembler-times "vcvttss2usi" 2 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "vcvttsd2usi" 2 { target { ! ia32 } } } } */
