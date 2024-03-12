#include <stdlib.h>

void test (void)
{
  void *p = malloc (sizeof (int));
  if (!p)
    return;
  int *q = (int *) p;
  if (!q)
    return;
  free (q);
}
