/* PR debug/50017 */
/* { dg-do compile } */
/* { dg-options "-O3 -fcompare-debug" } */
/* { dg-xfail-if "" { powerpc-ibm-aix* } } */

struct S { int r, i; };

void
foo (struct S *x, int y)
{
  int i;
  for (i = 1; i < y; i++)
    {
      struct S a, b, c;
      a = x[0];
      b = x[i];
      c.r = a.r * b.r - a.i * b.i;
      c.i = a.r * b.i + a.i * b.r;
      x[i] = c;
    }
}
