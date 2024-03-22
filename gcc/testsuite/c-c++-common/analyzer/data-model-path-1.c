#include <stddef.h>

static int *__attribute__((noinline))
callee (void)
{
  return NULL;
}

void test_1 (void)
{
  int *p = callee (); /* { dg-message "return of NULL to 'test_1' from 'callee'" } */
  *p = 42; /* { dg-warning "dereference of NULL 'p'" } */
}
