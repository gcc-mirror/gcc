/* { dg-require-effective-target vect_int } */
/* { dg-require-effective-target vect_float } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 20

int
main1 ()
{
  int i;
  float a[N];
  float e[N];
  float b[N] = {0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45};
  float c[N] = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15};
  float d[N] = {0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30};
  int ic[N] = {0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45};
  int ib[N] = {0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45};
  int ia[N];

  for (i = 0; i < N; i++)
    {
      a[i] = b[i] + c[i] + d[i];
      e[i] = b[i] + c[i] + d[i];
      ia[i] = ib[i] + ic[i];
    }

  /* check results:  */
  for (i = 0; i <N; i++)
    {
      float fres = b[i] + c[i] + d[i];
      int ires = ib[i] + ic[i];	
      if (a[i] != fres || e[i] != fres || ia[i] != ires)
        abort ();
    }

  return 0;
}

int main (void)
{
  check_vect ();

  return main1 ();
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
/* { dg-final { scan-tree-dump-times "Vectorizing an unaligned access" 0 "vect" } } */



