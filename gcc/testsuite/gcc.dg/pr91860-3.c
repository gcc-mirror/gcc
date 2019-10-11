/* { dg-do compile } */
/* { dg-options "-Og -g2 --param=max-combine-insns=3" } */

int a, b;

void
foo (void)
{
  unsigned short d = 46067;
  int e = e;
  d <<= __builtin_mul_overflow (~0, e, &a);
  d |= -68719476735;
  b = d;
}

