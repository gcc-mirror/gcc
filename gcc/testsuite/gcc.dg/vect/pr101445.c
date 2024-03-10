#include "tree-vect.h"

int a[35] = { 1, 1, 3 };

void __attribute__((noipa))
foo ()
{
  for (int b = 4; b >= 0; b--)
    {
      int tem = a[b * 5 + 3 + 1];
      a[b * 5 + 3] = tem;
      a[b * 5 + 2] = tem;
      a[b * 5 + 1] = tem;
      a[b * 5 + 0] = tem;
    }
}

int main()
{
  check_vect ();
  foo ();
#pragma GCC novector
  for (int d = 0; d < 25; d++)
    if (a[d] != 0)
      __builtin_abort ();
  return 0;
}
