#include <stdlib.h>

void test_1 (int x, int y, int *out)
{
  int *ptr = (int *)malloc (sizeof (int));
  if (!ptr)
    return;
  *ptr = 19;

  free (ptr);
  *out = *ptr; /* { dg-warning "use after 'free' of 'ptr'" } */
}
