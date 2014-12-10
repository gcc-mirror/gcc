/* { dg-do compile } */
/* { dg-require-effective-target ilp32 } */
/* { dg-options "-O2 -ffast-math -ftree-vectorize -msse2 -mtune=generic --param ggc-min-expand=0 --param ggc-min-heapsize=0" } */
/* This test, tests two thing, we vectorize square root and also we don't crash due to a GC issue.  */


extern double sqrt (double __x);
void calc_freq (int *dest)
{
  float tmp_out[257];
  int i;
  for (i = 0; i < 256; i++)
    dest[i] = sqrt (tmp_out[i]);
}

/* { dg-final { scan-assembler "sqrtpd" } } */
