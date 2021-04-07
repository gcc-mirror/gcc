/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 16

struct foostr {
  _Complex short f1;
  _Complex short f2;
};

_Complex short a1[64] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__)));
_Complex short a2[64] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__)));
_Complex short b1[64] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__)));
_Complex short b2[64] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__)));
struct foostr c[64] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__)));

__attribute__ ((noinline)) void
foo (void)
{
  int i;

  for (i = 0; i < N; i++)
    {
      c[i].f1 = a1[i] + b1[i];
      c[i].f2 = a2[i] + b2[i];
    }

}

int
main (void)
{ 
  int i;
  check_vect ();
  
  foo ();

  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorizing stmts using SLP" 0 "vect" { target vect_load_lanes } } } */
/* { dg-final { scan-tree-dump-times "vectorizing stmts using SLP" 2 "vect" { target { ! vect_load_lanes } xfail { ! vect_hw_misalign } } } } */
