/* { dg-do compile } */
/* { dg-options "-O2" } */

#define TEN(x) x x x x x x x x x x
#define THOUSAND(x) TEN (TEN (TEN (x)))

int
foo (int x, int y)
{
  register int a = y + 57;
  register int b = y + 31;

  while (x-- > 0)
    {
      THOUSAND (a += b; b -= a;)
    }
  return a + b;
}
