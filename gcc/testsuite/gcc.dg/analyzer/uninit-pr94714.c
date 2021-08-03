#include <stdio.h>

int main (void)
{
  int *p;
  int i;

  p = &i; /* { dg-bogus "uninitialized" } */
  printf ("%d\n", p[0]);  /* { dg-warning "use of uninitialized value '\\*p'" } */

  return 0;
}
