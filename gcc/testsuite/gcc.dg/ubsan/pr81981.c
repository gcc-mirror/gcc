/* PR sanitizer/81981 */
/* { dg-do compile } */
/* { dg-options "-O2 -Wmaybe-uninitialized -fsanitize=undefined -ffat-lto-objects" } */

int v;

int
foo (int i)
{
  int t[1], u[1];
  int n = 0;

  if (i)
    {
      t[n] = i;
      u[0] = i;
    }

  v = u[0];		/* { dg-warning "may be used uninitialized in this function" } */
  return t[0];		/* { dg-warning "may be used uninitialized in this function" } */
}
