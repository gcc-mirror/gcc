/* { dg-do run } */
/* { dg-options "-fsanitize=vla-bound" } */

#include <stdio.h>

/* Don't instrument the arrays here.  */
int
foo (int n, int a[])
{
  return a[n - 1];
}

int
main (void)
{
  fputs ("UBSAN TEST START\n", stderr);

  int a[6] = { };
  int ret = foo (3, a);

  fputs ("UBSAN TEST END\n", stderr);
  return ret;
}

/* { dg-output "UBSAN TEST START(\n|\r\n|\r)UBSAN TEST END" } */
