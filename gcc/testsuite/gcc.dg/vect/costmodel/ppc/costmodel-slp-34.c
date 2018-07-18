/* { dg-require-effective-target vect_int } */
/* { dg-skip-if "cost too high" { powerpc*le-*-* } } */

#include <stdarg.h>
#include "../../tree-vect.h"

struct mystr {
  int f1;
  int f2;
};

struct mystr af[16] = {
  10, 11, 12, 13, 14, 15, 16, 17,
  20, 21, 22, 23, 24, 25, 26, 27,
  30, 31, 32, 33, 34, 35, 36, 37,
  40, 41, 42, 43, 44, 45, 46, 47
};

struct mystr bf[16] = {
  12, 13, 14, 15, 16, 17, 18, 19,
  22, 23, 24, 25, 26, 27, 28, 29,
  32, 33, 34, 35, 36, 37, 38, 39,
  42, 43, 44, 45, 46, 47, 48, 49
};

struct mystr cf[16];

int res1[16] = {
  22, 26, 30, 34, 42, 46, 50, 54,
  62, 66, 70, 74, 82, 86, 90, 94,
};

int res2[16] = {
  24, 28, 32, 36, 44, 48, 52, 56,
  64, 68, 72, 76, 84, 88, 92, 96,
};

__attribute__ ((noinline)) void
foo (void)
{
  int i;

  for (i = 0; i < 16; i++)
  {
    cf[i].f1 = af[i].f1 + bf[i].f1;
    cf[i].f2 = af[i].f2 + bf[i].f2;
  }
}



int
main (void)
{ 
  int i;

  check_vect ();
  foo ();

  /* Check resiults. */ 
  for (i = 0; i < 16; i++)
    {
      if (cf[i].f1 != res1[i])
	abort ();
      if (cf[i].f2 != res2[i])
        abort ();

    }
  return 0;
} 

/* { dg-final { scan-tree-dump-times "vectorization not profitable" 0 "vect" } } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
/* { dg-final { scan-tree-dump-times "vectorizing stmts using SLP" 1 "vect"  } } */
