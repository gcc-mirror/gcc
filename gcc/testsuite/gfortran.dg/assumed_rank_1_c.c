/* Called by assumed_rank_1.f90.  */

#include <stdlib.h>  /* For abort().  */

struct array {
  int *data;
};

void check_value_ (struct array *b, int n, int val[])
{
  int i;

  for (i = 0; i < n; i++)
    if (b->data[i] != val[i])
      abort ();
}
