/* PR target/82524 */

struct S { unsigned char b, g, r, a; };
union U { struct S c; unsigned v; };

static inline unsigned char
foo (unsigned char a, unsigned char b)
{
  return ((a + 1) * b) >> 8;
}

__attribute__((noinline, noclone)) unsigned
bar (union U *x, union U *y)
{
  union U z;
  unsigned char v = x->c.a;
  unsigned char w = foo (y->c.a, 255 - v);
  z.c.r = foo (x->c.r, v) + foo (y->c.r, w);
  z.c.g = foo (x->c.g, v) + foo (y->c.g, w);
  z.c.b = foo (x->c.b, v) + foo (y->c.b, w);
  z.c.a = 0;
  return z.v;
}

int
main ()
{
  union U a, b, c;
  if ((unsigned char) ~0 != 255 || sizeof (unsigned) != 4)
    return 0;
  a.c = (struct S) { 255, 255, 255, 0 };
  b.c = (struct S) { 255, 255, 255, 255 };
  c.v = bar (&a, &b);
  if (c.c.b != 255 || c.c.g != 255 || c.c.r != 255 || c.c.a != 0)
    __builtin_abort ();
  return 0;
}
