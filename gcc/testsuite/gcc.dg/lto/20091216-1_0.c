/* { dg-lto-do run } */

#include "../nop.h"

asm (".globl start_\nstart_: " NOP);

int
main ()
{
  return 0;
}
