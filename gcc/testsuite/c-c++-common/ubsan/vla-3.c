/* { dg-do run } */
/* { dg-options "-fsanitize=vla-bound -fno-sanitize-recover=vla-bound" } */

/* Don't instrument the arrays here.  */
int
foo (int n, int a[])
{
  return a[n - 1];
}

int
main (void)
{
  int a[6] = { };
  int ret = foo (3, a);
  return ret;
}
