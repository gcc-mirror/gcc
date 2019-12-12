/* { dg-do compile } */
/* { dg-options "-O2 -ftree-slp-vectorize -fno-vect-cost-model -mno-sse" } */

int x;

void foo (short a, short b)
{
  ((short *)&x)[0] = a;
  ((short *)&x)[1] = b;
}

#if __SIZEOF_LONG__ == 8
long y;

void bar (short a, short b)
{
  ((short *)&y)[0] = a;
  ((short *)&y)[1] = b;
  ((short *)&y)[2] = a;
  ((short *)&y)[3] = b;
}
#endif
