/* { dg-do run } */
/* { dg-options "-fsanitize=nonnull-attribute" } */

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
  bar (&x, 42, 0, 1, &x, 17, 18);
  bar (0, 25, &x, 7, &x, 0, 8);
  bar (&x, -82, &x, 68, 0, 9, 0);
  foo (&x, 42, 0, 1, &x, 17, 18);
  foo (0, 25, &x, 7, &x, 0, 8);
  foo (&x, -82, &x, 68, 0, 9, 0);
}

/* { dg-output "\.c:21:\[0-9]*:\[^\n\r]*null pointer passed as argument 3, which is declared to never be null\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*\.c:21:\[0-9]*:\[^\n\r]*null pointer passed as argument 1, which is declared to never be null\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*\.c:21:\[0-9]*:\[^\n\r]*null pointer passed as argument 5, which is declared to never be null\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*\.c:31:\[0-9]*:\[^\n\r]*null pointer passed as argument 3, which is declared to never be null\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*\.c:32:\[0-9]*:\[^\n\r]*null pointer passed as argument 1, which is declared to never be null\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*\.c:33:\[0-9]*:\[^\n\r]*null pointer passed as argument 5, which is declared to never be null" } */
