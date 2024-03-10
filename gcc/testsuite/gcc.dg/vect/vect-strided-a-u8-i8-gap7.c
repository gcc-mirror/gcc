/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 16 

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
main1 ()
{
  int i;
  s arr[N];
  s *ptr = arr;
  s res[N];
  unsigned char u, t, s, x, z, w;

  for (i = 0; i < N; i++)
    {
      arr[i].a = i;
      arr[i].b = i * 2;
      arr[i].c = 17;
      arr[i].d = i+34;
      arr[i].e = i * 3 + 5;
      arr[i].f = i * 5;
      arr[i].g = i - 3;
      arr[i].h = 67;
      asm volatile ("" ::: "memory");
    }

  for (i = 0; i < N; i++)
    {
      u = ptr->b - ptr->a;
      t = ptr->d - ptr->c;
      res[i].c = u + t;
      x = ptr->b + ptr->d;
      res[i].a = ptr->a + x;
      res[i].d = u + t;
      s = ptr->h - ptr->a;
      res[i].b = s + t;
      res[i].f = ptr->f + ptr->h;
      res[i].e = ptr->b + ptr->e;
      res[i].h = ptr->d;
      res[i].g = u + t;
      ptr++;
    }

  /* check results:  */
#pragma GCC novector
  for (i = 0; i < N; i++)
    { 
      if (res[i].c != arr[i].b - arr[i].a + arr[i].d - arr[i].c
          || res[i].a != arr[i].a + arr[i].b + arr[i].d
          || res[i].d != arr[i].b - arr[i].a + arr[i].d - arr[i].c
          || res[i].b != arr[i].h - arr[i].a + arr[i].d - arr[i].c
          || res[i].f != arr[i].f + arr[i].h
          || res[i].e != arr[i].b + arr[i].e
          || res[i].h != arr[i].d
          || res[i].g != arr[i].b - arr[i].a + arr[i].d - arr[i].c)
         abort();
   }
}


int main (void)
{
  check_vect ();

  main1 ();

  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { target vect_strided8 } } } */
  
