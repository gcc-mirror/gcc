/* { dg-do run } */
/* { dg-options "-fsanitize=nonnull-attribute" } */

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
  bar (&x, 42, 0, 1, &x);
  bar (0, 25, &x, 7, &x);
  bar (&x, -82, &x, 68, 0);
  foo (&x, 42, 0, 1, &x);
  foo (0, 25, &x, 7, &x);
  foo (&x, -82, &x, 68, 0);
}

/* { dg-output "\.c:19:\[0-9]*:\[^\n\r]*null pointer passed as argument 3, which is declared to never be null\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*\.c:19:\[0-9]*:\[^\n\r]*null pointer passed as argument 1, which is declared to never be null\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*\.c:19:\[0-9]*:\[^\n\r]*null pointer passed as argument 5, which is declared to never be null\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*\.c:29:\[0-9]*:\[^\n\r]*null pointer passed as argument 3, which is declared to never be null\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*\.c:30:\[0-9]*:\[^\n\r]*null pointer passed as argument 1, which is declared to never be null\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*\.c:31:\[0-9]*:\[^\n\r]*null pointer passed as argument 5, which is declared to never be null" } */
