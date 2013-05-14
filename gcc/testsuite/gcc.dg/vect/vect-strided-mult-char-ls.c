/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 32 

typedef struct {
   unsigned char a;
   unsigned char b;
} s;

typedef struct {
   unsigned int a;
   unsigned int b;
} ii;

volatile int y = 0;

__attribute__ ((noinline)) int
main1 (s *arr, ii *iarr)
{
  s *ptr = arr;
  ii *iptr = iarr;
  s res[N];
  ii ires[N];
  int i;

  for (i = 0; i < N; i++)
    {
      ires[i].a = iptr->b;
      ires[i].b = iptr->a;
      res[i].b = ptr->b - ptr->a;
      res[i].a = ptr->b + ptr->a;
      iptr++;
      ptr++;
    }
  
  /* check results:  */
  for (i = 0; i < N; i++)
    {
      if (res[i].b != arr[i].b - arr[i].a 
          || ires[i].a != iarr[i].b
          || res[i].a != arr[i].b + arr[i].a
          || ires[i].b != iarr[i].a
)
	abort ();
    }

  return 0;
}

int main (void)
{
  int i;
  s arr[N];
  ii iarr[N];
  
  check_vect ();

  for (i = 0; i < N; i++)
    {
      arr[i].a = i;
      arr[i].b = i * 2;
      iarr[i].a = i;
      iarr[i].b = i * 3;
      if (y) /* Avoid vectorization.  */
        abort ();
    }

  main1 (arr, iarr);

  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect"  { target vect_strided2 } } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
  
