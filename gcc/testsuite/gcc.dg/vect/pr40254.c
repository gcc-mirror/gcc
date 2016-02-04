#include <stdarg.h>
#include "tree-vect.h"

struct s
{
  int *x;
  int x1;
  int x2;
  int x3;
  int *y;
};

struct s arr[64] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__)));

__attribute__ ((noinline)) void
foo (int i, int *in_x, int *in_y)
{
  arr[i].x = in_x;
  arr[i].y = in_y;
}

int
main (void)
{
  int a, b;

  check_vect ();

  foo (5, &a, &b);

  if (arr[5].x != &a || arr[5].y != &b)
    abort ();

  return 0;
}


