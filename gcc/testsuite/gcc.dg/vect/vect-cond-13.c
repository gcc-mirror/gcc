/* { dg-do run } */

#include "tree-vect.h"

int a[1024];
int b[1024];

int
foo ()
{
  int tem;
  for (int i = 0; i < 1024; ++i)
    {
      if (a[i] < 0)
        tem = -a[i] - 1;
      else
        tem = a[i];
      b[i] = tem + 10;
    }
  return tem;
}

int main()
{
  check_vect ();

  for (int i = 0; i < 1024; ++i)
    {
      a[i] = i - 333;
      __asm__ volatile ("" ::: "memory");
    }
  int res = foo ();
  if (res != 1023 - 333)
    abort ();
  return 0;
}

/* { dg-final { scan-tree-dump "vectorized 1 loops" "vect" { target vect_condition } } } */
