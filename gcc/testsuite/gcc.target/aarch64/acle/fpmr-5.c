/* { dg-options "-O" } */

#include <stdint.h>

void f(int cond)
{
  uint64_t x;
  asm volatile ("" : "=r" (x));
  if (cond)
    {
      register uint64_t fpmr asm ("fpmr") = x;
      asm volatile ("" :: "Umv" (fpmr));
    }
}

/* { dg-final { scan-assembler-not {\tsub\tsp,} } } */
