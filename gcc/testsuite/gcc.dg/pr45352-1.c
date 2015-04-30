/* { dg-do compile { target powerpc*-*-* ia64-*-* i?86-*-* x86_64-*-* } } */
/* { dg-options "-O3 -fschedule-insns -fschedule-insns2 -fselective-scheduling2 -fsel-sched-pipelining -funroll-loops -fprefetch-loop-arrays" } */
/* { dg-additional-options "-march=i686 -msse" { target { { i?86-*-* x86_64-*-* } && ia32 } } } */

void main1 (float *pa, float *pc)
{
  int i;
  float b[256];
  float c[256];
  for (i = 0; i < 256; i++)
    b[i] = c[i] = pc[i];
  for (i = 0; i < 256; i++)
    pa[i] = b[i] * c[i];
}
