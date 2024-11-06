/* { dg-do compile } */
/* { dg-additional-options "-msse4" { target { x86_64-*-* i?86-*-* } } } */

long x[2];

int
foo (int c)
{
  long x0 = x[0], x1 = x[1];
  int t = x0 != 0 | x1 != 0;
  c *= t;
  return c;
}
