/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -fno-vect-cost-model -mavx512f -mprefer-vector-width=512" } */

double a[1024], b[1024], c[1024];

void foo (int flag, int n)
{
  _Bool x = flag == 3;
  for (int i = 0; i < n; ++i)
    a[i] = (x ? b[i] : c[i]) * 42.;
}

/* { dg-final { scan-assembler-not "\[^x\]orl" } } */
