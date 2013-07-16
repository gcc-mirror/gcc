/* PR rtl-optimization/56903 */
/* { dg-do compile } */
/* { dg-options "-Os" } */
/* { dg-additional-options "-march=pentium3" { target ia32 } } */

int a, *b, c;
struct S { int s : 1; } *fn1 (void);
extern int fn3 (void), fn4 (int *);

void
fn2 (void)
{
  int e = fn3 ();
  char f = c + fn1 ()->s * 4;
  if (*b && f == e)
    a = *b;
  fn4 (b);
}
