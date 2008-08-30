/* Test whether using target specific options, we can generate the reciprocal
   square root instruction.  */
/* { dg-do compile } */
/* { dg-options "-O2 -march=k8 -mno-recip -mfpmath=sse -ffast-math" } */

float do_recip  (float a) __attribute__((__target__("recip")));
float do_normal (float a);

float do_recip  (float a) { return 1.0f / __builtin_sqrtf (a); }
float do_normal (float a) { return 1.0f / __builtin_sqrtf (a); }

/* { dg-final { scan-assembler "sqrtss" } } */
/* { dg-final { scan-assembler "rsqrtss" } } */
