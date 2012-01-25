/* { dg-options "-fgnu-tm" } */

#include <stdint.h>

extern int i;

main()
{
  __transaction_atomic { i = 0; }
}

#define noinline __attribute__((noinline,noclone,used))

noinline uint32_t _ITM_beginTransaction(uint32_t a, ...) { asm(""); }
noinline void _ITM_commitTransaction (void) { asm(""); }
noinline void _ITM_WU4 (void *a, uint32_t b) { asm(""); }
noinline void _ITM_WU8 (void *a, uint64_t b) { asm(""); }
noinline void _ITM_registerTMCloneTable (void) { asm(""); }
noinline void _ITM_deregisterTMCloneTable (void) { asm(""); }
