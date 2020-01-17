#include <stdlib.h>

/* Ensure that we don't need to laboriously walk every path to get
   to the end of the function.  */

int test_1 (int n)
{
  int i, j, k;
  k = 0;
  for (int i = 0; i < n; i++)
    for (int j = 0; j < 1000; j++)
      k++;
  return k;
}
