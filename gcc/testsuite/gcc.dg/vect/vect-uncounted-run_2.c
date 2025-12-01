/* execution test for the correct resetting of live out values on epilog loop
   entry.  */
/* { dg-add-options vect_early_break } */
/* { dg-do run } */
/* { dg-require-effective-target vect_early_break_hw } */
/* { dg-require-effective-target vect_int } */

#include <assert.h>
#include "tree-vect.h"
#define N 9

__attribute__((noipa, noinline))
void
test01 ()
{
    {
      int x[N] = {2, 4, 6, 8, 10, 12, 14, 16, 18};
      const int y[N] = {3, 5, 7, 9, 11, 13, 15, 17, 19};
      int z[N] = {5, 9, 13, 17, 21, 25, 29, 33, 37};

      int *x0 = x;
      int *xN = x+N;
      const int *y0 = y;
      const int *yN = y+N;

      int *res = x;

      for (; x0 != xN && y0 != yN; ++x0, (void)++y0, ++res)
	*res = *x0 + *y0;
      assert (x0 == x+N && y0 == y+N && res == x+N);
      assert (x[0] == z[0] && x[1] == z[1]);
    }
}

int
main ()
{
  test01 ();
}

/* { dg-final { scan-tree-dump "vectorized 1 loops in function" "vect" } } */
