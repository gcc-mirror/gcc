/* { dg-do compile } */
/* { dg-options "-O3 -msse2 -mno-avx" } */

void fill(unsigned char __seg_gs *arr, unsigned char c, long n)
{
  for (long i = 0; i < n; ++i)
    arr[i] = c;
}

/* { dg-final { scan-assembler "mov\[au\]p.\[ \t\]\[^,\]+, %gs:" } } */
