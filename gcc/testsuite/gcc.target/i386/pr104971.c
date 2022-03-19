/* PR middle-end/104971 */
/* { dg-do run } */
/* { dg-options "-O2" } */

#include <x86intrin.h>

__attribute__((noipa)) void
foo (void)
{
  __readeflags ();
}

int
main ()
{
  foo ();
  return 0;
}
