/* { dg-do run } */
/* { dg-require-effective-target arm_arch_v6t2_hw } */
/* { dg-add-options arm_arch_v6t2 } */

#include "arm_acle.h"

volatile uint32_t clz_in = 0x1234;
volatile uint32_t rev_in = 0x12345678;
volatile uint64_t rev64_in = 0x1234567890abcdef;

int
main (int argc, char **argv)
{
  if (__clz(clz_in) != 19) { __builtin_abort(); }
  if (__clzl(clz_in) != 19) { __builtin_abort(); }
  if (__clzll(clz_in) != 51) { __builtin_abort(); }
  if (__cls(clz_in) != 18) { __builtin_abort(); }
  if (__clsl(clz_in) != 18) { __builtin_abort(); }
  if (__clsll(clz_in) != 50) { __builtin_abort(); }
  if (__rev(rev_in) != 0x78563412) { __builtin_abort(); }
  if (__revl(rev_in) != 0x78563412) { __builtin_abort(); }
  if (__revll(rev64_in) != 0xefcdab9078563412) { __builtin_abort(); }
  if (__rev16(rev_in) != 0x34127856) { __builtin_abort(); }
  if (__rev16l(rev_in) != 0x34127856) { __builtin_abort(); }
  if (__rev16ll(rev64_in) != 0x34127856ab90efcd) { __builtin_abort(); }
  if (__revsh(clz_in) != 0x3412) { __builtin_abort(); }
  return 0;
}
