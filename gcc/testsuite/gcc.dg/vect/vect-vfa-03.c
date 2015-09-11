/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 16
struct S
{
  unsigned short a;
  unsigned short b;
};

struct S result[N] = {20, 13, 22, 14, 24, 15, 26, 16, 28, 17, 30, 18,
                      32, 19, 34, 20, 36, 21, 38, 22, 40, 23, 42, 24,
		      44, 25, 46, 26, 48, 27, 50, 28};
struct S X[N] = {10, 10, 11, 11, 12, 12, 13, 13, 14, 14, 15, 15, 16,
                 16, 17, 17, 18, 18, 19, 19, 20, 20, 21, 21, 22, 22,
		 23, 23, 24, 24, 25, 25};
struct S Y[N] = {};
 
__attribute__ ((noinline)) void
foo (struct S * in, struct S * out)
{
  int i;

  for (i = 0; i < N; i++)
    {
      out[i].a = in[i].a * 2;
      out[i].b = in[i].b + 3;
    }
}

int
main (void)
{ 
  int i;

  check_vect ();

  foo (X, Y);
  
  /* check results:  */
  for (i = 0; i < N; i++)
    {
      if (Y[i].a != result[i].a)  
	abort ();

      if (Y[i].b != result[i].b) 
	abort ();

    }
  return 0;
} 

/* Needs interleaving support.  */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { target vect_strided2 } } } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 0 "vect" { xfail  vect_strided2 } } } */
