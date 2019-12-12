#include "tree-vect.h"

int a[28];
int main()
{
  check_vect ();
  a[4] = 1;
  int c = 1;
  for (int b = 0; b < 8; b++)
    if (a[b])
      c = 0;
  if (c)
    abort();
  return 0;
}
