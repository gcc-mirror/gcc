/* { dg-do compile } */
/* { dg-options "-march=x86-64-v3 -O2 -mno-avx512f" } */
/* { dg-final { scan-assembler-not "vpand" } } */
/* { dg-final { scan-assembler-not "65535" } } */

void
foo (int* a, short* __restrict b, int* c)
{
    for (int i = 0; i != 16; i++)
      b[i] = c[i] + a[i];
}

void
foo1 (int* a, short* __restrict b, int* c)
{
    for (int i = 0; i != 8; i++)
      b[i] = c[i] + a[i];
}
