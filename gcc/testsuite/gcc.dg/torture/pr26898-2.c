/* { dg-do run } */

#include <limits.h>

int a = 0, b = INT_MAX - 1;
extern void abort(void);
int main()
{
  if (a - 1 > b + 1)
    abort();
  return 0;
}
