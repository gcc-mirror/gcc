/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 128 

typedef struct {
   unsigned short a;
   unsigned short b;
   unsigned short c;
   unsigned short d;
} s;

__attribute__ ((noinline)) int
main1 ()
{
  s arr[N];
  s *ptr = arr;
  s res[N];
  int i;

  for (i = 0; i < N; i++)
    {
      arr[i].a = i;
      arr[i].b = i * 2;
      arr[i].c = 17;
      arr[i].d = i+34;
      asm volatile ("" ::: "memory");
    }

  for (i = 0; i < N; i++)
    {
      unsigned short x, y, z, w;
      x = ptr->b - ptr->a;
      y = ptr->d - ptr->c;
      res[i].c = x + y;
      z =  ptr->a + ptr->c;
      w = ptr->b + ptr->d;
      res[i].a = z + w;
      res[i].d = x + y;
      res[i].b = x + y;
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
  
  check_vect ();

  main1 ();

  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect"  { target vect_strided4 } } } */
  
