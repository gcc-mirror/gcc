#include "tree-vect.h"

int a[4], b[4];

void __attribute__((noipa))
foo(int x, int y)
{
  int tem0 = x + 1;
  int tem1 = y + 2;
  int tem2 = x + 3;
  int tem3 = y + 4;
  a[0] = tem0 + b[1];
  a[1] = tem1 + b[0];
  a[2] = tem2 + b[2];
  a[3] = tem3 + b[3];
}

int main()
{
  check_vect ();

  b[0] = 10;
  b[1] = 14;
  b[2] = 18;
  b[3] = 22;
  foo (-1, -3);
  if (a[0] != 14 || a[1] != 9 || a[2] != 20 || a[3] != 23)
    __builtin_abort ();
  return 0;
}
