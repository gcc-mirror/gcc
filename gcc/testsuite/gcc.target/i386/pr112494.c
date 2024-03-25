/* PR target/112494 */
/* { dg-options "-Og -fno-tree-copy-prop -fno-tree-fre -fno-tree-ccp -fno-tree-forwprop" } */

#include <x86intrin.h>

int main()
{
  long flags = 0xD7;

  __writeeflags(0xD7);
  flags && !__readeflags();

  if ((flags && (!__readeflags())) != 0xD7)
    ;

  return 0;
}
