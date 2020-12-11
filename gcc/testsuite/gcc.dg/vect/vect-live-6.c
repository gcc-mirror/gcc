#include "tree-vect.h"

int a[1024];
int b[1024];

_Bool
fn1 ()
{
  _Bool tem;
  for (int i = 0; i < 1024; ++i)
    {
      tem = !a[i];
      b[i] = tem;
    }
  return tem;
}

int main()
{
  check_vect ();
  for (int i = 0; i < 1024; ++i)
    {
      a[i] = i & 5;
      __asm__ volatile ("" ::: "memory");
    }
  if (fn1 () != !(1023 & 5) || b[2] != 1)
    abort ();
  return 0;
}

/* { dg-final { scan-tree-dump "vectorized 1 loops" "vect" { target { vect_int && vect_condition } } } } */
