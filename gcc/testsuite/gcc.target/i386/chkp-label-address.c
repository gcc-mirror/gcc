/* { dg-do compile { target { ! x32 } } } */
/* { dg-options "-fcheck-pointer-bounds -mmpx -O2 -Wchkp" } */

#include <stdio.h>

static int f1 () /* { dg-warning "function cannot be instrumented" "" } */
{
  static int array = &&label_B - &&label_A;

 label_A:

  printf ("%d\n", array);

 label_B:

  return 0;
}

int f2 (int i)
{
  printf ("%d\n", i);
  return f1 ();
}
