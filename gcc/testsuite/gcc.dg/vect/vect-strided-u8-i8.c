/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include <stdio.h>
#include "tree-vect.h"

#define N 32 

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
main1 (s *arr)
{
  int i;
  s *ptr = arr;
  s res[N];
  unsigned char u, t, s, x, y, z, w;

  for (i = 0; i < N; i++)
    {
      u = ptr->b - ptr->a;
      t = ptr->d - ptr->c;
      res[i].c = u + t;
      s =  ptr->a + ptr->g;
      x = ptr->b + ptr->d;
      res[i].a = s + x;
      res[i].d = u + t;
      s = ptr->h - ptr->a;
      x = ptr->d - ptr->c;
      res[i].b = s + x;
      res[i].f = ptr->f + ptr->h;
      res[i].e = ptr->b + ptr->e; 
      res[i].h = ptr->d - ptr->g;   
      res[i].g = u + t;
      ptr++; 
    } 
   
  /* check results:  */
  for (i = 0; i < N; i++)
    { 
      if (res[i].c != arr[i].b - arr[i].a + arr[i].d - arr[i].c
          || res[i].a != arr[i].a + arr[i].g + arr[i].b + arr[i].d
          || res[i].d != arr[i].b - arr[i].a + arr[i].d - arr[i].c
          || res[i].b != arr[i].h - arr[i].a + arr[i].d - arr[i].c
          || res[i].f != arr[i].f + arr[i].h
          || res[i].e != arr[i].b + arr[i].e
          || res[i].h != arr[i].d - arr[i].g
          || res[i].g != arr[i].b - arr[i].a + arr[i].d - arr[i].c
       )
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
      arr[i].e = i;
      arr[i].f = i + 5;
      arr[i].g = i + 3;
      arr[i].h = 67;
      if (arr[i].a == 178)
         abort(); 
    } 

  main1 (arr);

  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { target { vect_interleave && vect_extract_even_odd } } } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
  
