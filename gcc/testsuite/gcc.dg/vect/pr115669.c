/* { dg-additional-options "-fwrapv" } */

#include "tree-vect.h"

int a = 10;
unsigned b;
long long c[100];
int foo()
{
  long long *d = c;
  for (short e = 0; e < a; e++)
    b += ~(d ? d[e] : 0);
  return b;
}

int main()
{
  check_vect ();
  if (foo () != -10)
    abort ();
  return 0;
}
