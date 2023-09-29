#include <stdlib.h>

void *
calls_malloc (void)
{
  void *result = malloc (1024);
  return result;
}

int *test_1 (int i)
{
  int *ptr = (int *)calls_malloc ();
  *ptr = i; /* { dg-warning "dereference of possibly-NULL 'ptr'" } */
  return ptr;
}

/* Same as test_1, to exercise the caches.  */

int *test_2 (int i)
{
  int *ptr = (int *)calls_malloc ();
  *ptr = i; /* { dg-warning "dereference of possibly-NULL 'ptr'" } */
  return ptr;
}
