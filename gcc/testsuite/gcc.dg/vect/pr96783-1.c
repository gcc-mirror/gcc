/* { dg-do run } */

#include "tree-vect.h"

void __attribute__((noipa))
foo (long *a, int off, int n)
{
  for (int i = 0; i < n; ++i)
    {
      long tem1 = a[0];
      long tem2 = a[1];
      long tem3 = a[2];
      long tem4 = a[off + 1];
      a[0] = tem4;
      long tem5 = a[off + 2];
      a[1] = tem5;
      long tem6 = a[off + 3];
      a[2] = tem6;
      a[off + 1] = tem1;
      a[off + 2] = tem2;
      a[off + 3] = tem3;
      a -= 3;
    }
}

int main ()
{
  long a[3 * 9];
  check_vect ();
  for (int i = 0; i < 3 * 9; ++i)
    a[i] = i;
  foo (a + 3 * 5, 6-1, 5);
  const long b[3 * 8] = { 0, 1, 2, 21, 22, 23, 18, 19, 20, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17 };
  for (int i = 0; i < 3 * 8; ++i)
    if (a[i] != b[i])
      __builtin_abort ();
  return 0;
}
