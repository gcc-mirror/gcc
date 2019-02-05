/* { dg-do compile } */

#include <stdlib.h>
#include <assert.h>

struct dc
{
  int a;
  int **b;
};

int
main ()
{
  int n = 100, i, j;
  struct dc v = { .a = 3 };

  v.b = (int **) malloc (sizeof (int *) * n);
  for (i = 0; i < n; i++)
    v.b[i] = (int *) malloc (sizeof (int) * n);

#pragma acc parallel loop copy(v.a, v.b[:n][:n]) /* { dg-error "dynamic arrays cannot be used within structs" } */
  for (i = 0; i < n; i++)
    for (j = 0; j < n; j++)
      v.b[i][j] = v.a + i + j;

  for (i = 0; i < n; i++)
    for (j = 0; j < n; j++)
      assert (v.b[i][j] == v.a + i + j);

  return 0;
}
