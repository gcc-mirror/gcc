#include "static-3.h"
int bar(int *a)
{
  int i, tot;
  for (i = tot = 0; i < 100; i++)
    tot += a[i];
  return tot;
}
