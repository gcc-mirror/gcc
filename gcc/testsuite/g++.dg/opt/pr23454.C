/* PR rtl-optimization/23454 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

void foo ();
int a, b;
char c;
long long d, e;

static inline int
bar (const long long s, const long long t)
{
  return ((s < t) ? -1 : s > t ? 1 : 0);
}

int fn ();
int f;

void
baz (int x)
{
  long long g = fn ();
  if (f)
    {
      b++;
      return;
    }
  if (g == 0)
    a++;
  if (x)
    foo ();
  if (!c)
    c = 1;
  else if (g != 0)
    {
      if (bar (g, d) < 0)
	d = g;
      if (bar (g, e) > 0)
	e = g;
    }
}
