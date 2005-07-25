/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 16

struct s{
  int b[N];
  int c[N];
  int m;
};

struct t{
  struct s strc_s;
  int m;
};

struct test1{
  struct t strc_t;
  struct t *ptr_t;
  int k;
  int l;
};

int main1 ()
{
  int i;
  struct test1 tmp1;
  int a[N] = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15};
  struct t tmp2;

  tmp1.ptr_t = &tmp2;

  /* DR bases comparison: record and array.  */
  for (i = 0; i < N; i++)
    {
      tmp1.strc_t.strc_s.b[i] = a[i];
    }

  /* Check results.  */
  for (i = 0; i < N; i++)
    {
      if (tmp1.strc_t.strc_s.b[i] != a[i])
	abort();
    }

  /* DR bases comparison: record containing ptr and array.  */
  for (i = 0; i < N; i++)
    {
      tmp1.ptr_t->strc_s.c[i] = a[i];
    }

  /* Check results.  */
  for (i = 0; i < N; i++)
    {
      if (tmp1.ptr_t->strc_s.c[i] != a[i])
	abort();
    }

    
  return 0;
}
       
int main (void)
{
  check_vect ();
    
  return main1 ();
}
          
/* { dg-final { scan-tree-dump-times "vectorized 2 loops" 1 "vect" } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
  

