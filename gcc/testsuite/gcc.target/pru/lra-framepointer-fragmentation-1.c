/* { dg-do assemble } */
/* { dg-options "-O1 -fno-omit-frame-pointer" } */
#include <stdint.h>

extern uint64_t global;

uint64_t __attribute__((noinline)) test(uint64_t a, uint64_t b,
                                         uint64_t c, uint64_t d,
                                         uint64_t e, uint64_t f,
                                         uint64_t g, uint64_t h)
{
  uint64_t l1 = 0x12345678, l2 = 0x87654321, l3 = 1001, l4 = 1002;
  uint64_t l5 = 1004;
  uint32_t l6 = 2005;
  uint16_t s1 = 4321;
  uint8_t c1 = 101, c2 = 102;

  /* The numerous dummy asm input operands create just
   * enough register pressure to resort to using
   * FP.b1 (r4.b1).
   */

  asm ("nop" /* { dg-error "'asm' operand has impossible constraints" } */
       : "=r" (l1)
       : "0" (l1), "r" (a), "r"(b),
       "r"(c), "r"(d), "r"(e), "r"(f),
       "r"(g), "r"(h), "r"(l2),
       "r"(c1), "r"(c2), "r"(s1),
       "r"(l3), "r"(l4), "r"(l5), "r"(l6));

  global = a+b+c+d+e+f+g+h + s1 + c1+c2 + l2;

  return l1;
}
