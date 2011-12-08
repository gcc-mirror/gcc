/* PR lto/48437 */
/* { dg-do run } */
/* { dg-options "-g" } */

#include "../nop.h"

int i __attribute__((used));
int main()
{
  volatile int i;
  for (i = 3; i < 7; ++i)
    {
      extern int i;
      asm volatile (NOP : : : "memory"); /* { dg-final { gdb-test 14 "i" "0" } } */
    }
  return 0;
}
