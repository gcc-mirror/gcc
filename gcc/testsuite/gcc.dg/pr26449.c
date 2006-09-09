/* { dg-do compile { target i?86-*-* } } */
/* { dg-require-effective-target ilp32 } */
/* { dg-options "-O1 -ftree-vectorize -march=pentium4 -std=c99" } */

void matmul_i4 (int bbase_yn, int xcount)
{
  int x;
  int * restrict dest_y;
  const int * abase_n;

  for (x = 0;  x < xcount;  x++)
    {
      dest_y[x] += abase_n[x] * bbase_yn;
    }
}
