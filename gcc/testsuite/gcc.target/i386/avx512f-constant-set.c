/* { dg-do compile } */
/* { dg-options "-O3 -march=skylake-avx512" } */
/* { dg-final { scan-assembler-not "%zmm\[0-9\]+" } } */

void
avx512f_test (short *table)
{
  int i;
  for (i = 0; i < 128; ++i)
    table[i] = -1;
}
