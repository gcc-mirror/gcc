/* { dg-do compile } */
/* { dg-options "-O3 -fpic" } */

/* Clobber highs do not need to be spilled around tls usage.  */

typedef float v4si __attribute__ ((vector_size (16)));

__thread v4si tx;

v4si foo (v4si a, v4si b, v4si c)
{
  v4si y;

  y = a + tx + b + c;

  return y + 7;
}

/* { dg-final { scan-assembler-not {\tst[rp]\t[dqv]} } } */
