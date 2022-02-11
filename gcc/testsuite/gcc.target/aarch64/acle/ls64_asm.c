/* { dg-do compile } */
/* { dg-options "-march=armv8-a+ls64 -O2" } */

#ifndef __ARM_FEATURE_LS64
#error "__ARM_FEATURE_LS64 is not defined but should be!"
#endif

/* Inline assembly for LS64 instructions.  */

#include <arm_acle.h>

void
ls64_load (data512_t *output, const void *addr)
{
    __asm__ volatile ("ld64b %0, [%1]"
                      : "=r" (*output)
                      : "r" (addr)
                      : "memory");
}

/* { dg-final { scan-assembler-times {ld64b } 1 } } */

void
ls64_store (const data512_t *input, void *addr)
{
    __asm__ volatile ("st64b %1, [%0]"
                      : /* No outputs.  */
                      : "r" (addr), "r" (*input)
                      : "memory");
}

/* { dg-final { scan-assembler-times {st64b } 1 } } */

uint64_t
ls64_store_v (const data512_t *input, void *addr)
{
    uint64_t status;
    __asm__ volatile ("st64bv %0, %2, [%1]"
                      : "=r" (status)
                      : "r" (addr), "r" (*input)
                      : "memory");
    return status;
}

/* { dg-final { scan-assembler-times {st64bv } 1 } } */

uint64_t
ls64_store_v0 (const data512_t *input, void *addr)
{
    uint64_t status;
    __asm__ volatile ("st64bv0 %0, %2, [%1]"
                      : "=r" (status)
                      : "r" (addr), "r" (*input)
                      : "memory");
    return status;
}

/* { dg-final { scan-assembler-times {st64bv0 } 1 } } */
