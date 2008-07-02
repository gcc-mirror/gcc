/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "../../tree-vect.h"

#define N 17
struct test {
  char ca[N];
};

extern struct test s;
 
int main1 ()
{  
  int i;

  for (i = 0; i < N; i++)
    {
      s.ca[i] = 5;
    }

  /* check results:  */
  for (i = 0; i < N; i++)
    {
      if (s.ca[i] != 5)
        abort ();
    }

  return 0;
}

int main (void)
{ 
  return main1 ();
} 

/* { dg-final { scan-tree-dump-times "vectorization not profitable" 0 "vect" } } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
