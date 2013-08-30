/* { dg-do run } */
/* { dg-options "-fsanitize=integer-divide-by-zero -Wno-overflow" } */

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

/* { dg-output "division of -2147483648 by -1 cannot be represented in type int(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*division of -2147483648 by -1 cannot be represented in type int(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*division of -2147483648 by -1 cannot be represented in type int(\n|\r\n|\r)" } */
