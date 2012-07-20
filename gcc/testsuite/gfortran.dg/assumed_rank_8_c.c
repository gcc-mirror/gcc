/* Called by assumed_rank_8.f90 and assumed_rank_9.f90.  */

#include <stdlib.h>  /* For abort().  */

struct a {
  int *dat;
};

struct b {
  struct a _data;
};


void check_ (struct a *x)
{
  if (*x->dat != 489)
    abort ();
}


void check2_ (struct b *x)
{
  if (*x->_data.dat != 489)
    abort ();
}
