/* { dg-do run } */
/* { dg-options "-fsanitize=undefined -fno-sanitize-recover=undefined" } */

__attribute__((noipa, nonnull_if_nonzero (1, 4, 7)))
__attribute__((nonnull (3), nonnull_if_nonzero (5, 2, 6))) void
foo (void *a, unsigned long b, void *c, int d, void *e, unsigned long f, int g)
{
  (void) a;
  (void) b;
  (void) c;
  (void) d;
  (void) e;
  (void) f;
  (void) g;
}

__attribute__((noipa))
void
bar (void *a, unsigned long b, void *c, int d, void *e, unsigned long f, int g)
{
  foo (a, b, c, d, e, f, g);
}

int
main ()
{
  char x;
  bar (&x, 42, &x, 1, &x, 2, 3);
  bar (0, 0, &x, 0, 0, 0, 0);
  bar (0, 5, &x, 4, 0, 0, 0);
  bar (0, 0, &x, 0, 0, 6, 7);
}
