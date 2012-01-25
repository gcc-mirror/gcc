/* { dg-lto-options {{-flto -fgnu-tm}} } */
/* { dg-lto-do link } */
/* { dg-require-effective-target stdint_types } */

#include <stdint.h>

extern void foobar() __attribute__((transaction_callable));

#define noinline __attribute__((noinline,noclone,used))

noinline uint32_t _ITM_beginTransaction(uint32_t a, ...) { asm(""); }
noinline void _ITM_commitTransaction (void) { asm(""); }
noinline void _ITM_WU4 (void *a, uint32_t b) { asm(""); }
noinline void _ITM_WU8 (void *a, uint64_t b) { asm(""); }
noinline void _ITM_registerTMCloneTable (void) { asm(""); }
noinline void _ITM_deregisterTMCloneTable (void) { asm(""); }

main()
{
  __transaction_relaxed
    {
      foobar();
    }
}
