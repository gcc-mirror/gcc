/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 128 

typedef struct {
   int a;
   int b;
   int c;
   int d;
} s;

__attribute__ ((noinline)) int
main1 (s *arr)
{
  int i;
  s *ptr = arr;
  s res[N];

  for (i = 0; i < N; i++)
    {
      res[i].c = ptr->b - ptr->a + ptr->d - ptr->c;
      res[i].a = ptr->a + ptr->c + ptr->b + ptr->d;
      res[i].d = ptr->b - ptr->a + ptr->d - ptr->c;
      res[i].b = ptr->b - ptr->a + ptr->d - ptr->c;
      ptr++;
    }
  
  /* check results:  */
#pragma GCC novector
  for (i = 0; i < N; i++)
    {
      if (res[i].c != arr[i].b - arr[i].a + arr[i].d - arr[i].c
          || res[i].a != arr[i].a + arr[i].c + arr[i].b + arr[i].d
          || res[i].d != arr[i].b - arr[i].a + arr[i].d - arr[i].c
          || res[i].b != arr[i].b - arr[i].a + arr[i].d - arr[i].c)
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
      arr[i].c = 17;
      arr[i].d = i+34;
      asm volatile ("" ::: "memory");
    } 

  main1 (arr);

  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect"  { target vect_strided4 } } } */
  
