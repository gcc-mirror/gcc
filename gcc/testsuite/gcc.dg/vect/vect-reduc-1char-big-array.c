/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 256

unsigned char ub[N];
unsigned char uc[N];
unsigned char diff;

volatile int y = 0;

__attribute__ ((noinline)) void
main1 (unsigned char x, unsigned char max_result, unsigned char min_result)
{
  int i;
  unsigned char udiff = 2;
  unsigned char umax = x;
  unsigned char umin = x;

  diff = 2;
  for (i = 0; i < N; i++) {
    ub[i] = i;
    uc[i] = i;
    if (i%16 == 0)
      {
	ub[i] = i+2;
	diff += 2;
      }
    if (uc[i] > max_result)
      max_result = uc[i];
    if (uc[i] < min_result)
      min_result = uc[i];

    /* Avoid vectorization.  */
    if (y)
      abort ();
  }
  for (i = 0; i < N; i++) {
    udiff += (unsigned char) (ub[i] - uc[i]);
  }

  for (i = 0; i < N; i++) {
    umax = umax < uc[i] ? uc[i] : umax;
  }

  for (i = 0; i < N; i++) {
    umin = umin > uc[i] ? uc[i] : umin;
  }

  /* check results:  */
  if (udiff != diff)
    abort ();
  if (umax != max_result)
    abort ();
  if (umin != min_result)
    abort ();
}

int main (void)
{
  check_vect ();

  main1 (100, 100, 1);
  main1 (0, 15, 0);
  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 3 loops" 1 "vect" { xfail vect_no_int_min_max } } } */
