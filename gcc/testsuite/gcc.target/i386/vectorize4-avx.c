/* { dg-do compile } */
/* { dg-options "-O2 -ffast-math -ftree-vectorize -mavx -mtune=generic" } */


extern double sqrt (double __x);
calc_freq (int *dest)
{
  float tmp_out[257];
  int i;
  for (i = 0; i < 256; i++)
    dest[i] = sqrt (tmp_out[i]);
}

/* { dg-final { scan-assembler "vsqrtpd\[ \\t\]+\[^\n\]*%ymm" { xfail *-*-* } } } */
