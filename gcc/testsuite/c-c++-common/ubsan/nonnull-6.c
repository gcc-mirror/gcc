/* { dg-do run } */
/* { dg-options "-fsanitize=undefined -fno-sanitize-recover=undefined" } */

__attribute__((noipa, nonnull_if_nonzero (1, 4)))
__attribute__((nonnull (3), nonnull_if_nonzero (5, 2))) void
foo (void *a, unsigned long b, void *c, int d, void *e)
{
  (void) a;
  (void) b;
  (void) c;
  (void) d;
  (void) e;
}

__attribute__((noipa))
void
bar (void *a, unsigned long b, void *c, int d, void *e)
{
  foo (a, b, c, d, e);
}

int
main ()
{
  char x;
  bar (&x, 42, &x, 1, &x);
  bar (0, 0, &x, 0, 0);
}
