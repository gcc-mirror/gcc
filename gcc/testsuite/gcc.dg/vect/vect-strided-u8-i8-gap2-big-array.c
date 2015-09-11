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

s check_res[N];

volatile int y = 0;

__attribute__ ((noinline)) int
main1 (s *arr)
{
  int i;
  s *ptr = arr;
  s res[N];

  for (i = 0; i < N; i++)
    {
      res[i].c = ptr->b;
      res[i].a = ptr->f + ptr->b;
      res[i].d = ptr->f - ptr->b;
      res[i].b = ptr->f;
      res[i].f = ptr->b;
      res[i].e = ptr->f - ptr->b;
      res[i].h = ptr->f;
      res[i].g = ptr->f - ptr->b;
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
  int i;
  s arr[N];

  check_vect ();

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

      check_res[i].c = arr[i].b;
      check_res[i].a = arr[i].f + arr[i].b;
      check_res[i].d = arr[i].f - arr[i].b;
      check_res[i].b = arr[i].f;
      check_res[i].f = arr[i].b;
      check_res[i].e = arr[i].f - arr[i].b;
      check_res[i].h = arr[i].f;
      check_res[i].g = arr[i].f - arr[i].b;

      if (y) /* Avoid vectorization.  */
        abort ();
    }

  main1 (arr);

  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { target vect_strided8 } } } */

