/* PR sanitizer/114956 */
/* { dg-do compile } */
/* { dg-options "-O2 -fsanitize=address,null" } */

int **a;
void qux (int *);

__attribute__((always_inline)) static inline int *
foo (void)
{
  int b[1];
  qux (b);
  return a[1];
}

__attribute__((no_sanitize_address)) void
bar (void)
{
  *a = foo ();
}

void
baz (void)
{
  bar ();
}
