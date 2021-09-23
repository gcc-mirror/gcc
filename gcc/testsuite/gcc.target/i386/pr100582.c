/* { dg-do compile } */
/* { dg-options "-O3 -mavx2" } */

typedef unsigned char v32qi __attribute__((vector_size(32)));

v32qi
f2 (v32qi x, v32qi a, v32qi b)
{
  v32qi e;
  for (int i = 0; i != 32; i++)
    e[i] = x[i] ? a[i] : b[i];

  return e;
}

/* { dg-final { scan-assembler-times "pblend" 1 } } */
