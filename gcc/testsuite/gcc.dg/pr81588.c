/* PR tree-optimization/81588 */
/* { dg-do run } */
/* { dg-options "-O2" } */

long long int a = 5011877430933453486LL, c = 1;
unsigned short b = 24847;

#include "tree-ssa/pr81588.c"

int
main ()
{
  foo ();
  if (c != 0)
    __builtin_abort ();
  a = 24846;
  c = 1;
  foo ();
  if (c != 1)
    __builtin_abort ();
  a = -5;
  foo ();
  if (c != 0)
    __builtin_abort ();
  return 0;
}
