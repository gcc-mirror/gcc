#include <stdlib.h>

void *
calls_malloc (void)
{
  void *result = malloc (1024); /* { dg-message "allocated here" } */
  return result; /* { dg-warning "leak of 'result'" } */
}

void test_1 ()
{
  calls_malloc (); /* { dg-message "calling 'calls_malloc' from 'test_1'" } */
}

static void callee (int i)
{
}

void test_2 (int i)
{
  callee (i);
}
