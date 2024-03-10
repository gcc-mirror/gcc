/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 128 

typedef struct {
   int a;
   int b;
   int c;
   int d;
   int e;
} s;

int
main1 (s *arr)
{
  int i;
  s *ptr = arr;
  s res[N];

  /* SLP with unrolling by 4.  */
  for (i = 0; i < N; i++)
    {
      res[i].c = ptr->c + ptr->c;
      res[i].a = ptr->a + ptr->a;
      res[i].d = ptr->d + ptr->d;
      res[i].b = ptr->b + ptr->b;
      res[i].e = ptr->e + ptr->e; 
      ptr++; 
    } 
   
  /* check results:  */
#pragma GCC novector
  for (i = 0; i < N; i++)
    { 
      if (res[i].c != arr[i].c + arr[i].c
          || res[i].a != arr[i].a + arr[i].a
          || res[i].d != arr[i].d + arr[i].d
          || res[i].b != arr[i].b + arr[i].b
          || res[i].e != arr[i].e + arr[i].e)
         abort();
    }

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
      arr[i].e = i * 3 + 5;
      asm volatile ("" ::: "memory");
    } 

  main1 (arr);

  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
/* { dg-final { scan-tree-dump-times "vectorizing stmts using SLP" 1 "vect" { target {! vect_strided5 } } } } */
  
