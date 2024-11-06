/* PR tree-optimization/71109 */
/* { dg-additional-options "-std=gnu17" } */
/* { dg-require-effective-target non_strict_prototype } */

struct S { int g, h; signed char i; int j; signed char k; int l[4]; } a, c;
struct T { signed char g; } e;
int *b, d;
static void foo ();

void
bar (void)
{
  while (d)
    {
      int k;
      struct T f[3];
      foo (bar, a);
      for (k = 0;; k++)
	f[k] = e;
    }
}

static inline void
foo (int x, struct S y, struct T z)
{
  for (z.g = 2; z.g; z.g--)
    {
      c = a = y;
      *b |= 6;
      if (y.g)
	break;
    }
}
