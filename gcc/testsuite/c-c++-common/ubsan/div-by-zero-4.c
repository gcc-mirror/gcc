/* { dg-do run } */
/* { dg-options "-fsanitize=integer-divide-by-zero -Wno-overflow" } */

#include <limits.h>

int
main (void)
{
  /* This should not fail.  */
  return (unsigned int) INT_MIN / -1;
}
