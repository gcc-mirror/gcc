/* { dg-require-effective-target vect_int } */
/* { dg-require-effective-target vect_float } */
/* { dg-add-options bind_pic_locally } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 64

  int ia[N];
  int ib[N]= 
    {1,1,0,0,1,0,1,0,
     1,1,0,0,1,0,1,0,
     1,1,0,0,1,0,1,0,
     1,1,0,0,1,0,1,0,
     1,1,0,0,1,0,1,0,
     1,1,0,0,1,0,1,0,
     1,1,0,0,1,0,1,0,
     1,1,0,0,1,0,1,0};

  signed char ca[N];
  signed char cb[N] =
    {1,1,0,0,1,0,1,0,
     1,1,0,0,1,0,1,0,
     1,1,0,0,1,0,1,0,
     1,1,0,0,1,0,1,0,
     1,1,0,0,1,0,1,0,
     1,1,0,0,1,0,1,0,
     1,1,0,0,1,0,1,0,
     1,1,0,0,1,0,1,0};

  short sa[N];
  short sb[N] =
    {1,1,0,0,1,0,1,0,
     1,1,0,0,1,0,1,0,
     1,1,0,0,1,0,1,0,
     1,1,0,0,1,0,1,0,
     1,1,0,0,1,0,1,0,
     1,1,0,0,1,0,1,0,
     1,1,0,0,1,0,1,0,
     1,1,0,0,1,0,1,0};

  float fa[N];
  float fb[N] =
    {1,1,0,0,1,0,1,0,
     1,1,0,0,1,0,1,0,
     1,1,0,0,1,0,1,0,
     1,1,0,0,1,0,1,0,
     1,1,0,0,1,0,1,0,
     1,1,0,0,1,0,1,0,
     1,1,0,0,1,0,1,0,
     1,1,0,0,1,0,1,0};

__attribute__ ((noinline)) int
main1 ()
{
  int i;
  /* Check ints.  */

  for (i = 0; i < N; i++)
    {
      ia[i] = -ib[i];
    }

  /* check results:  */
  for (i = 0; i <N; i++)
    {
      if (ia[i] != -ib[i])
        abort ();
    }

  /* Check chars.  */

  for (i = 0; i < N; i++)
    {
      ca[i] = -cb[i];
    }

  /* check results:  */
  for (i = 0; i <N; i++)
    {
      if (ca[i] != -cb[i])
        abort ();
    }

  /* Check shorts.  */

  for (i = 0; i < N; i++)
    {
      sa[i] = -sb[i];
    }

  /* check results:  */
  for (i = 0; i <N; i++)
    {
      if (sa[i] != -sb[i])
        abort ();
    }

  /* Check floats.  */

  for (i = 0; i < N; i++)
    {
      fa[i] = -fb[i];
    }

  /* check results:  */
  for (i = 0; i <N; i++)
    {
      if (fa[i] != -fb[i])
        abort ();
    }

  return 0;
}

int main (void)
{
  check_vect ();

  return main1 ();
}

/* { dg-final { scan-tree-dump-times "vectorized 4 loops" 1 "vect" } } */
/* { dg-final { scan-tree-dump-times "Vectorizing an unaligned access" 0 "vect" } } */

