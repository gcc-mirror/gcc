/* { dg-do compile } */
/* { dg-options "-march=x86-64-v3 -mno-avx512f -O2" } */
/* { dg-final { scan-assembler-not "vpsra[dw]" } } */

void
foo2 (char* __restrict a, short* b)
{
  for (int i = 0; i != 32; i++)
    a[i] = b[i] >> (short)8;
}

void
foo3 (char* __restrict a, short* b)
{
  for (int i = 0; i != 16; i++)
    a[i] = b[i] >> (short)8;
}

