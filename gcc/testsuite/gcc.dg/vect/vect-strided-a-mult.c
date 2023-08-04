/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 128

typedef struct {
   unsigned short a;
   unsigned short b;
} s;

typedef struct {
   unsigned int a;
   unsigned int b;
} ii;

__attribute__ ((noinline)) int
main1 ()
{
  s arr[N];
  s *ptr = arr;
  ii iarr[N];
  ii *iptr = iarr;
  s res[N];
  ii ires[N];
  int i;

  for (i = 0; i < N; i++)
    {
      arr[i].a = i;
      arr[i].b = i * 2;
      iarr[i].a = i;
      iarr[i].b = i * 3;
      asm volatile ("" ::: "memory");
    }

  for (i = 0; i < N; i++)
    {
      ires[i].a = iptr->b - iptr->a;
      ires[i].b = iptr->b + iptr->a;
      res[i].b = ptr->b - ptr->a;
      res[i].a = ptr->b + ptr->a;
      iptr++;
      ptr++;
    }

  /* check results:  */
#pragma GCC novector
  for (i = 0; i < N; i++)
    {
      if (res[i].b != arr[i].b - arr[i].a
          || ires[i].a != iarr[i].b - iarr[i].a
          || res[i].a != arr[i].b + arr[i].a
          || ires[i].b != iarr[i].b +  iarr[i].a
)
        abort ();
    }

  return 0;
}

int main (void)
{
  int i;

  check_vect ();

  main1 ();

  return 0;
}   

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect"  { target vect_strided2 } } } */

