/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 16

struct foostr {
  _Complex short f1;
  _Complex short f2;
};

struct foostr a[16] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__))) =
  {
    11 + 23i, 24 + 22i,
    11 + 26i, 24 + 35i,
    19 + 20i, 29 + 14i,
    23 + 31i, 26 + 30i,
    29 + 39i, 24 + 18i,
    20 + 32i, 16 + 23i,
    13 + 26i, 37 + 34i,
    12 + 23i, 26 + 14i,
    36 + 14i, 31 + 17i,
    35 + 17i, 17 + 36i,
    13 + 34i, 19 + 12i,
    27 + 34i, 36 + 19i,
    21 + 39i, 16 + 33i,
    28 + 18i, 39 + 26i,
    32 + 27i, 13 + 38i,
    35 + 36i, 34 + 28i,
  };

struct foostr b[16] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__))) =
  {
    37 + 12i, 23 + 15i,
    14 + 11i, 13 + 25i,
    35 + 29i, 22 + 34i,
    24 + 34i, 16 + 39i,
    34 + 32i, 26 + 21i,
    34 + 36i, 11 + 37i,
    25 + 21i, 10 + 39i,
    10 + 36i, 35 + 22i,
    39 + 29i, 23 + 21i,
    34 + 33i, 39 + 14i,
    16 + 31i, 32 + 33i,
    20 + 14i, 35 + 30i,
    26 + 24i, 36 + 37i,
    31 + 20i, 32 + 28i,
    25 + 27i, 15 + 30i,
    10 + 31i, 37 + 37i,
  };
struct foostr c[16] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__)));
struct foostr res[N] =
  {
    48 + 35i, 47 + 37i,
    25 + 37i, 37 + 60i,
    54 + 49i, 51 + 48i,
    47 + 65i, 42 + 69i,
    63 + 71i, 50 + 39i,
    54 + 68i, 27 + 60i,
    38 + 47i, 47 + 73i,
    22 + 59i, 61 + 36i,
    75 + 43i, 54 + 38i,
    69 + 50i, 56 + 50i,
    29 + 65i, 51 + 45i,
    47 + 48i, 71 + 49i,
    47 + 63i, 52 + 70i,
    59 + 38i, 71 + 54i,
    57 + 54i, 28 + 68i,
    45 + 67i, 71 + 65i,
  };

__attribute__ ((noinline)) void
foo (void)
{
  int i;

  for (i = 0; i < N; i++)
    {
      c[i].f1 = a[i].f1 + b[i].f1;
      c[i].f2 = a[i].f2 + b[i].f2;
    }

}

int
main (void)
{ 
  int i;
  check_vect ();
  
  foo ();

  /* check results:  */
  for (i = 0; i < N; i++)
    {
      if (c[i].f1 != res[i].f1)
	abort ();
      if (c[i].f2 != res[i].f2)
	abort ();
    }

  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
/* { dg-final { scan-tree-dump-times "vectorizing stmts using SLP" 1 "vect" } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
