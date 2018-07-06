/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 256

__attribute__ ((noinline))
void main1 (signed char x, signed char max_result, signed char min_result)
{
  int i;
  signed char b[N];
  signed char c[N];
  signed char check_diff = 2;
  signed char diff = 2;
  signed char max = x;
  signed char min = x;

  check_diff = 2;
  for (i = 0; i < N; i++) {
    b[i] = i;
    c[i] = i;
    if (i%16 == 0)
      {
	c[i] = i + 1;
	check_diff += 1;
      }
    if (c[i] > max_result)
      max_result = c[i];
    if (c[i] < min_result)
      min_result = c[i];
    asm volatile ("" ::: "memory");
  }

  for (i = 0; i < N; i++) {
    diff += (signed char) (c[i] - b[i]);
  }

  for (i = 0; i < N; i++) {
    max = max < c[i] ? c[i] : max;
  }

  for (i = 0; i < N; i++) {
    min = min > c[i] ? c[i] : min;
  }

  /* check results:  */
  if (diff != check_diff)
    abort ();
  if (max != max_result)
    abort ();
  if (min != min_result)
    abort ();
}

int main (void)
{
  check_vect ();

  main1 (100, 100, 1);
  main1 (0, 15, 0);
  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 3 loops" 1 "vect" { xfail *-*-* } } } */
