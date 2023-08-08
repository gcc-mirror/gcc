/* { dg-do compile } */

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

typedef struct {
  int *ptr;
} S;

int main(void)
{
#pragma omp declare mapper(grid: S x) map(([9][11]) x.ptr[3:3:2][1:4:3])
  S q;
  q.ptr = (int *) calloc (9 * 11, sizeof (int));

  /* The 'grid' mapper specifies a noncontiguous region, so it can't be used
     for 'map' like this.  */
#pragma omp target enter data map(mapper(grid), to: q)
/* { dg-error {array section is not contiguous in .map. clause} "" { target *-*-* } .-1 } */
/* { dg-error {.#pragma omp target enter data. must contain at least one .map. clause} "" { target *-*-* } .-2 } */

#pragma omp target
  for (int i = 0; i < 9*11; i++)
    q.ptr[i] = i;

  /* It's OK on a 'target update' directive though.  */
#pragma omp target update from(mapper(grid): q)

  for (int j = 0; j < 9; j++)
    for (int i = 0; i < 11; i++)
      if (j >= 3 && j <= 7 && ((j - 3) % 2) == 0
	  && i >= 1 && i <= 10 && ((i - 1) % 3) == 0)
	assert (q.ptr[j * 11 + i] == j * 11 + i);

#pragma omp target exit data map(mapper(grid), release: q)
/* { dg-error {array section is not contiguous in .map. clause} "" { target *-*-* } .-1 } */
/* { dg-error {.#pragma omp target exit data. must contain at least one .map. clause} "" { target *-*-* } .-2 } */
  return 0;
}
