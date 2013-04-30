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

volatile int y = 0;

__attribute__ ((noinline)) int
main1 ()
{
  int i;
  s arr[N];
  s *ptr = arr;
  s res[N];

  for (i = 0; i < N; i++)
    {
      arr[i].a = i;
      arr[i].b = i * 2;
      arr[i].c = 17;
      arr[i].d = i+34;
      arr[i].e = i + 5;
      arr[i].f = i * 2 + 2;
      arr[i].g = i - 3;
      arr[i].h = 56;
      if (y) /* Avoid vectorization.  */
        abort ();
    }

  for (i = 0; i < N; i++)
    {
      res[i].c = ptr->a;
      res[i].a = ptr->f + ptr->a;
      res[i].d = ptr->f - ptr->a;
      res[i].b = ptr->f;
      res[i].f = ptr->a;
      res[i].e = ptr->f - ptr->a; 
      res[i].h = ptr->f;   
      res[i].g = ptr->f - ptr->a;
      ptr++; 
    } 
   
  /* check results:  */
  for (i = 0; i < N; i++)
    { 
      if (res[i].c != arr[i].a
          || res[i].a != arr[i].f + arr[i].a
          || res[i].d != arr[i].f - arr[i].a
          || res[i].b != arr[i].f
          || res[i].f != arr[i].a
          || res[i].e != arr[i].f - arr[i].a
          || res[i].h != arr[i].f
          || res[i].g != arr[i].f - arr[i].a)
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
/* { dg-final { cleanup-tree-dump "vect" } } */
  
