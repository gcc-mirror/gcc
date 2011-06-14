/* PR rtl-optimization/49390 */

struct S { unsigned int s1; unsigned int s2; };
struct T { unsigned int t1; struct S t2; };
struct U { unsigned short u1; unsigned short u2; };
struct V { struct U v1; struct T v2; };
struct S a;
char *b;
union { char b[64]; struct V v; } u;
volatile int v;
extern void abort (void);

__attribute__((noinline, noclone)) void
foo (int x, void *y, unsigned int z, unsigned int w)
{
  if (x != 4 || y != (void *) &u.v.v2)
    abort ();
  v = z + w;
  v = 16384;
}

__attribute__((noinline, noclone)) void
bar (struct S x)
{
  v = x.s1;
  v = x.s2;
}

__attribute__((noinline, noclone)) int
baz (struct S *x)
{
  v = x->s1;
  v = x->s2;
  v = 0;
  return v + 1;
}

__attribute__((noinline, noclone)) void
test (struct S *c)
{
  struct T *d;
  struct S e = a;
  unsigned int f, g;
  if (c == 0)
    c = &e;
  else
    {
      if (c->s2 % 8192 <= 15 || (8192 - c->s2 % 8192) <= 31)
	foo (1, 0, c->s1, c->s2);
    }
  if (!baz (c))
    return;
  g = (((struct U *) b)->u2 & 2) ? 32 : __builtin_offsetof (struct V, v2);
  f = c->s2 % 8192;
  if (f == 0)
    {
      e.s2 += g;
      f = g;
    }
  else if (f < g)
    {
      foo (2, 0, c->s1, c->s2);
      return;
    }
  if ((((struct U *) b)->u2 & 1) && f == g)
    {
      bar (*c);
      foo (3, 0, c->s1, c->s2);
      return;
    }
  d = (struct T *) (b + c->s2 % 8192);
  if (d->t2.s1 >= c->s1 && (d->t2.s1 != c->s1 || d->t2.s2 >= c->s2))
    foo (4, d, c->s1, c->s2);
  return;
}

int
main ()
{
  struct S *c = 0;
  asm ("" : "+r" (c) : "r" (&a));
  u.v.v2.t2.s1 = 8192;
  b = u.b;
  test (c);
  if (v != 16384)
    abort ();
  return 0;
}
