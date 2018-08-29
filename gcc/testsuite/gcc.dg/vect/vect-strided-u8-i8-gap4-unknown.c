/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 160 

typedef struct {
   unsigned char a;
   unsigned char b;
   unsigned char c;
   unsigned char d;
   unsigned char e;
   unsigned char f;
   unsigned char g;
   unsigned char h;
} s;

__attribute__ ((noinline)) int
main1 (s *arr, int n)
{
  int i;
  s *ptr = arr;
  s res[N];
  unsigned char x;

  for (i = 0; i < N; i++)
    {
      res[i].a = 0;
      res[i].b = 0;
      res[i].c = 0;
      res[i].d = 0;
      res[i].e = 0;
      res[i].f = 0;
      res[i].g = 0;
      res[i].h = 0;
      __asm__ volatile ("");
    }

  /* Check peeling for gaps for unknown loop bound.  */
  for (i = 0; i < n; i++)
    {
      res[i].c = ptr->b + ptr->c;
      x = ptr->c + ptr->f;
      res[i].a = x + ptr->b;
      res[i].d = ptr->b + ptr->c;
      res[i].b = ptr->c;
      res[i].f = ptr->f + ptr->e;
      res[i].e = ptr->b + ptr->e; 
      res[i].h = ptr->c;   
      res[i].g = ptr->b + ptr->c;
      ptr++; 
    } 
   
  /* check results:  */
  for (i = 0; i < n; i++)
    { 
      if (res[i].c != arr[i].b + arr[i].c
          || res[i].a != arr[i].c + arr[i].f + arr[i].b
          || res[i].d != arr[i].b + arr[i].c
          || res[i].b != arr[i].c
          || res[i].f != arr[i].f + arr[i].e
          || res[i].e != arr[i].b + arr[i].e
          || res[i].h != arr[i].c
          || res[i].g != arr[i].b + arr[i].c)
        abort ();
   }

  /* Check also that we don't do more iterations than needed.  */
  for (i = n; i < N; i++)
    {
      if (res[i].c == arr[i].b + arr[i].c
          || res[i].a == arr[i].c + arr[i].f + arr[i].b
          || res[i].d == arr[i].b + arr[i].c
          || res[i].b == arr[i].c
          || res[i].f == arr[i].f + arr[i].e
          || res[i].e == arr[i].b + arr[i].e
          || res[i].h == arr[i].c
          || res[i].g == arr[i].b + arr[i].c)
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
      arr[i].a = 5;
      arr[i].b = 6;
      arr[i].c = 17;
      arr[i].d = 3;
      arr[i].e = 16;
      arr[i].f = 16;
      arr[i].g = 3;
      arr[i].h = 56;
      asm volatile ("" ::: "memory");
    } 

  main1 (arr, N-2);

  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { target vect_strided8 } } } */
  
