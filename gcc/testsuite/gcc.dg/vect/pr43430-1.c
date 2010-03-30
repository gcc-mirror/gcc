/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 16

typedef int uint8_t;
uint8_t data_ch1[N + 1] =
  { 0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30 };
uint8_t data_ch2[N + 1] =
  { 0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30 };
#define SUM 480

__attribute__ ((noinline)) int
foo (uint8_t * s1, uint8_t * s2, int stride)
{
  int score = 0;
  int x;
  for (x = 0; x < N; x++)
    score += ((s1[x] - s1[x + stride] + s2[x + stride]) >= 0 ?
	      s1[x] + s2[x + stride] :
	      s2[x + stride]);

  if (score != SUM)
    abort ();

  return 0;
}

int
main (void)
{
  check_vect ();
  return foo (data_ch1, data_ch2, 1);
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
