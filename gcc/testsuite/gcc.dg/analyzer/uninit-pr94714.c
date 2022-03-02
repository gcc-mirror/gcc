#include <stdio.h>

int main (void)
{
  int *p;
  int i; /* { dg-message "region created on stack here" } */

  p = &i; /* { dg-bogus "uninitialized" } */
  printf ("%d\n", p[0]);  /* { dg-warning "use of uninitialized value '\\*p'" } */

  return 0;
}
