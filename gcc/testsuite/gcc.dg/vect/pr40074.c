/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 16 

typedef struct {
   int a;
   int b;
   int c;
   int d;
} s;


s arr[N] = {{7,0,1,5}, {7,2,3,5}, {7,4,5,5}, {7,6,7,5}, {7,8,9,5}, {7,10,11,5}, {7,12,13,5}, {7,14,15,5}, {7,16,17,5}, {7,18,19,5}, {7,20,21,5}, {7,22,23,5}, {7,24,25,5}, {7,26,27,5}, {7,28,29,5}, {7,30,31,5}};

__attribute__ ((noinline)) int
main1 ()
{
  s *p = arr, *q = arr + 1;
  int res[N];
  int i;

  for (i = 0; i < N-1; i++)
    {
      res[i] = p->b + p->d + q->b;
      p++;
      q++;
    }
  
  /* check results:  */
  for (i = 0; i < N-1; i++)
    {
      if (res[i] != arr[i].b + arr[i].d + arr[i+1].b)
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

/* { dg-final { cleanup-tree-dump "vect" } } */
  
