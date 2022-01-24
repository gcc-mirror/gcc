/* { dg-do run } */
/* { dg-options "-fsanitize=integer-divide-by-zero -fno-sanitize-recover=undefined,float-divide-by-zero -Wno-overflow" } */

#include <limits.h>

int
main (void)
{
  volatile int min = INT_MIN;
  volatile int zero = 0;

  INT_MIN / -1;
  min / -1;
  min / (10 * zero - (2 - 1));

  return 0;
}
