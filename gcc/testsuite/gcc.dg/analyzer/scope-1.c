#include <stdlib.h>

int test_1 (void)
{
  {
    int *q = malloc (1024);
  }

  return 42; /* { dg-warning "leak of 'q'" } */
  // FIXME: would be better to report it at the close-of-scope
}

int test_2 (void)
{
  {
    void *q = malloc (1024);
  }

  int q = 42;

  return q; /* { dg-warning "leak of 'q'" } */
  // FIXME: would be better to report it at the close-of-scope
}
