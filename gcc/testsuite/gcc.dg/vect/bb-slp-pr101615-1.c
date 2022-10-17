/* { dg-do run } */
/* { dg-additional-options "-w -Wno-psabi" } */

#include "tree-vect.h"

typedef int v4si __attribute__((vector_size(16)));

int a[4];
int b[4];

void __attribute__((noipa))
foo (v4si x)
{
  b[0] = a[3] + x[0];
  b[1] = a[2] + x[1];
  b[2] = a[1] + x[2];
  b[3] = a[0] + x[3];
}

int main()
{
  check_vect ();
  for (int i = 0; i < 4; ++i)
    a[i] = i;
  v4si x = (v4si) { 8, 6, 4, 2 };
  foo (x);
  if (b[0] != 11 || b[1] != 8 || b[2] != 5 || b[3] != 2)
    __builtin_abort ();
  return 0;
}
