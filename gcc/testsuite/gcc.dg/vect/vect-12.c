/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 16

int main1 ()
{
  int i;
  int ia[N];
  int ic[N] = {0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45};
  int ib[N] = {0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45};
  short sa[N];
  short sc[N] = {0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45};
  short sb[N] = {0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45};

  /* Not vectorizable yet (multiple types with different nunits in vector).  */
  for (i = 0; i < N; i++)
    {
      ia[i] = ib[i] + ic[i];
      sa[i] = sb[i] + sc[i];
    }

  /* check results:  */
  for (i = 0; i < N; i++)
    {
      if (ia[i] != ib[i] + ic[i] || sa[i] != sb[i] + sc[i])
        abort ();
    }

  return 0;
}

int main (void)
{ 
  check_vect ();
  
  return main1 ();
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { xfail *-*-* } } } */
