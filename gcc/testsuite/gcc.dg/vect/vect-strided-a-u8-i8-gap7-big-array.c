/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 128

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
  s check_res[N];
  s res[N];
  unsigned char u, t, s, x, y, z, w;

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

      u = arr[i].b - arr[i].a;
      t = arr[i].d - arr[i].c;
      check_res[i].c = u + t;
      x = arr[i].b + arr[i].d;
      check_res[i].a = arr[i].a + x;
      check_res[i].d = u + t;
      s = arr[i].h - arr[i].a;
      check_res[i].b = s + t;
      check_res[i].f = arr[i].f + arr[i].h;
      check_res[i].e = arr[i].b + arr[i].e;
      check_res[i].h = arr[i].d;
      check_res[i].g = u + t;

      if (y) /* Avoid vectorization.  */
        abort ();
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
  for (i = 0; i < N; i++)
    {
      if (res[i].a != check_res[i].a
	  || res[i].b != check_res[i].b
	  || res[i].c != check_res[i].c
	  || res[i].d != check_res[i].d
	  || res[i].e != check_res[i].e
	  || res[i].f != check_res[i].f
	  || res[i].g != check_res[i].g
	  || res[i].h != check_res[i].h)
         abort ();
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

