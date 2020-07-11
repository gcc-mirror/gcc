/* { dg-do run } */
/* { dg-options "-O1 -fomit-frame-pointer" } */
#include <stdint.h>

extern void abort (void);

uint64_t global = 5;

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
   * enough register pressure to resort to using FP (r4).
   */

  asm ("ldi32 %0, 0x11223344\n\t"
       "add %0, %0, %2\n\t"
       "add %0, %0, %3\n\t"
       "add %0, %0, %4\n\t"
       "add %0, %0, %5\n\t"
       "add %0, %0, %6\n\t"
       "add %0, %0, %7\n\t"
       "add %0, %0, %8\n\t"
       "add %0, %0, %9\n\t"
       "add %0, %0, %10\n\t"
       "add %0, %0, %11\n\t"
       "add %0, %0, %12\n\t"
       "add %0, %0, %13\n\t"
       "add %0, %0, %14\n\t"
       "add %0, %0, %15\n\t"
       "add %0, %0, %16\n\t"
       "add %0, %0, %17\n\t"
       : "=r" (l1)
       : "0" (l1), "r" (a), "r"(b),
       "r"(c), "r"(d), "r"(e), "r"(f),
       "r"(g), "r"(h), "r"(c1), "r"(c2), "r"(s1),
       "r"(l2), "r"(l3), "r"(l4), "r"(l5), "r"(l6));

  global = a+b+c+d+e+f+g+h + s1 + c1+c2 + l2+l3+l4+l5+l6;

  return l1;
}

int main()
{
  uint64_t a = test(1, 2, 3, 4, 5, 6, 7, 8);

  if (a != 0x98879bc9) {
    abort();
  }
  if (global != 0x87656885) {
    abort();
  }
  return 0;
}
