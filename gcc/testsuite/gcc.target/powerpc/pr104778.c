/* PR debug/104778 */
/* { dg-do compile } */
/* { dg-options "-mcmpb -Og -g" } */
/* { dg-additional-options "-fpie" { target pie } } */

unsigned long long int p;
short int m, n;

void
foo (double u, int v, int x, int y, int z)
{
  long long int a = v;
  short int b = v;
  int c = 0, d = m, e = u;

  if (n)
    {
      int q = b;

      while (p / 1.0)
        c = 0;

      if (n * n == (d + 1) / (1LL << x))
        a = 1;

      b = u;
      while (d)
        {
          u = m + 1ULL;
          b = a - (unsigned long long int) u + a + (char) (u + 1.0);
          d = (v - 1LL) * n / d + q + x;
          q = m;
        }
    }

  while (c < 1)
    {
      int r;

      if (m == y)
        m = e * z;

      e = !a;

      while (!r)
        ;

      if (b)
        m = d;
    }
}
