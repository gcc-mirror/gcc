/* { dg-do compile } */
/* { dg-additional-options "-ftree-vectorize -fno-vect-cost-model" } */
/* { dg-additional-options "-mavx512f" { target x86_64-*-* i?86-*-* } } */

static inline int
foo (int y, int a)
{
  return (y && a) ? a : 0;
}

void
bar (int *__restrict a, int *__restrict d, int *__restrict e, int i)
{
  while (i < 1)
    {
      e[8] = e[7] = e[6] = e[5] = e[4] = e[3] = e[2] = e[1] = e[0]
        = foo (d[8], a[8]);
      e[9] = foo (d[9], a[9]);
      e[10] = foo (d[0], a[0]);
      e[11] = foo (d[1], a[1]);
      e[12] = foo (d[12], a[12]);
      e[13] = foo (d[13], a[13]);
      e[14] = foo (d[4], a[4]);
      e[15] = foo (d[15], a[15]);

      a += 16;
      e += 1;
      i += 1;
    }
}
