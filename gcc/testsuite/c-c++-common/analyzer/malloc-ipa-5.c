/* { dg-skip-if "requires hosted libstdc++ for stdlib malloc" { ! hostedlib } } */

#include <stdlib.h>

static int *calls_malloc(void)
{
  return (int *) malloc(sizeof(int));
}

int *test(void)
{
  int *p = calls_malloc(); /* { dg-message "possible return of NULL to 'test' from 'calls_malloc'" } */
  *p = 42; /* { dg-warning "dereference of possibly-NULL 'p'" } */
  return p;
}
