/* { dg-do run } */
/* { dg-options "-fsanitize=vla-bound -w" } */

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
  return foo (3, a);
}
