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
  unsigned char x;

  for (i = 0; i < N; i++)
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

  ptr = arr;
  /* Not vectorizable: gap in store.  */
  for (i = 0; i < N; i++)
    {
      res[i].a = ptr->b;
      res[i].b = ptr->c;
      ptr++;
    }

  /* Check results.  */
  for (i = 0; i < N; i++)
    {
      if (res[i].a != arr[i].b
	  || res[i].b != arr[i].c)
          abort ();
    }

}


int main (void)
{
  int i;
  s arr[N];
  unsigned char x;

  check_vect ();

  for (i = 0; i < N; i++)
    {
      arr[i].a = i;
      arr[i].b = i * 2;
      arr[i].c = 17;
      arr[i].d = i+34;
      arr[i].e = i * 3 + 5;
      arr[i].f = i * 5;
      arr[i].g = i - 3;
      arr[i].h = 56;

      check_res[i].c = arr[i].b + arr[i].c;
      x = arr[i].c + arr[i].f;
      check_res[i].a = x + arr[i].b;
      check_res[i].d = arr[i].b + arr[i].c;
      check_res[i].b = arr[i].c;
      check_res[i].f = arr[i].f + arr[i].e;
      check_res[i].e = arr[i].b + arr[i].e;
      check_res[i].h = arr[i].c;
      check_res[i].g = arr[i].b + arr[i].c;

      if (y) /* Avoid vectorization.  */
        abort ();
    }
  main1 (arr);

  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { target vect_strided8 } } } */
/* { dg-final { cleanup-tree-dump "vect" } } */

