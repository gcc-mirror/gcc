/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 128

typedef struct {
   unsigned short a;
   unsigned short b;
} s;

volatile int y = 0;

__attribute__ ((noinline)) int
main1 ()
{
  s arr[N];
  s *ptr = arr;
  unsigned int iarr[N];
  unsigned int *iptr = iarr;
  s res[N];
  unsigned int ires[N];
  int i;

  for (i = 0; i < N; i++)
    {
      arr[i].a = i;
      arr[i].b = i * 2;
      iarr[i] = i * 3;
      if (y) /* Avoid vectorization.  */
        abort ();
    }

  for (i = 0; i < N; i++)
    {
      ires[i] = *iptr;
      res[i].b = ptr->b - ptr->a;
      res[i].a = ptr->b + ptr->a;
      iptr++;
      ptr++;
    }

  /* check results:  */
  for (i = 0; i < N; i++)
    {
      if (res[i].b != arr[i].b - arr[i].a
          || ires[i] != iarr[i]
          || res[i].a != arr[i].b + arr[i].a)
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

