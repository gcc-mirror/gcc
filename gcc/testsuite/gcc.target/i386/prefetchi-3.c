/* { dg-do compile } */
/* { dg-options "-mprefetchi -O2" } */
/* { dg-final { scan-assembler-not "prefetchit0" } } */
/* { dg-final { scan-assembler-not "prefetchit1" } } */

#include <x86intrin.h>

void* p;

void extern
prefetchi_test1 (void)
{
  __builtin_ia32_prefetchi (p, 2); /* { dg-warning "instruction prefetch applies when in 64-bit mode with RIP-relative addressing and option '-mprefetchi'; they stay NOPs otherwise" } */
}

void extern
prefetchi_test2 (void)
{
  __builtin_ia32_prefetch (p, 0, 3, 1); /* { dg-warning "instruction prefetch applies when in 64-bit mode with RIP-relative addressing and option '-mprefetchi'; they stay NOPs otherwise" } */
} 
