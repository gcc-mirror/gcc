/* PR target/64882 */
/* { dg-do compile } */

int a, d, e;
long long b;
static long long *c = &b;

void
fn1 (short p)
{
}

long long
fn2 (long long p1, long long p2)
{
  return (p1 && p1 > 26854775807LL - p2) || p1 < -p2 ? p1 : p1 + p2;
}

void
fn3 ()
{
  long long f;
  int g = 3;
  int *h = &a;
  for (e = 0; e < 2; e++)
    {
      int *i = &g;
      if (!fn2 (*c, 7 < d % (*i)--))
	f = fn2 ((*h <= 0) | b, 5278350700LL);
      *h = f;
      fn1 (*h);
    }
}
