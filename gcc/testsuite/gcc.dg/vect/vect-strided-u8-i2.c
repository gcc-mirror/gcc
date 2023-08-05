/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 64

typedef struct {
   unsigned char a;
   unsigned char b;
} s;

__attribute__ ((noinline)) int
main1 (s *arr)
{
  s *ptr = arr;
  s res[N];
  int i;

  for (i = 0; i < N; i++)
    {
      res[i].a = ptr->b - ptr->a;
      res[i].b = ptr->b + ptr->a;
      ptr++;
    }
  /* check results:  */
#pragma GCC novector
  for (i = 0; i < N; i++)
    {
      if (res[i].a != arr[i].b - arr[i].a
          || res[i].b != arr[i].a + arr[i].b)
	abort ();
    }

  return 0;
}

int main (void)
{
  int i;
  s arr[N];
  
  check_vect ();

  for (i = 0; i < N; i++)
    { 
      arr[i].a = i;
      arr[i].b = i * 2;
      asm volatile ("" ::: "memory");
    } 

  main1 (arr);

  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect"  { target vect_strided2 } } } */
   
