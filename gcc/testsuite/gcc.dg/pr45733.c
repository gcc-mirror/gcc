/* { dg-do compile } */
/* { dg-options "-O -fstrict-overflow -ftree-vectorize" } */

typedef __INTPTR_TYPE__ intptr_t;

intptr_t
foo (void **p, int i)
{
  intptr_t x = 0;
  while (i--)
    x ^= (intptr_t) p[i];
  return x;
}
