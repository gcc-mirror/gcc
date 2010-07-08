/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 128

typedef struct {
   unsigned int a;
   unsigned int b;
} ii;

__attribute__ ((noinline)) int
main1 (unsigned short *arr, ii *iarr)
{
  unsigned short *ptr = arr;
  ii *iptr = iarr;
  unsigned short res[N];
  ii ires[N];
  int i;

  for (i = 0; i < N; i++)
    {
      ires[i].a = iptr->b - iptr->a;
      ires[i].b = iptr->b + iptr->a;
      res[i] = *ptr;
      iptr++;
      ptr++;
    }

  /* check results:  */
  for (i = 0; i < N; i++)
    {
      if (res[i] != arr[i]
          || ires[i].a != iarr[i].b - iarr[i].a
          || ires[i].b != iarr[i].b +  iarr[i].a)
        abort ();
    }

  return 0;
}

int main (void)
{
  int i;
  unsigned short arr[N];
  ii iarr[N];

  check_vect ();

  for (i = 0; i < N; i++)
    {
      arr[i] = i;
      iarr[i].a = i;
      iarr[i].b = i * 3;
      __asm__ volatile ("");
    }
  main1 (arr, iarr); 
    
  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect"  { target { vect_interleave && vect_extract_even_odd } } } } */
/* { dg-final { cleanup-tree-dump "vect" } } */

