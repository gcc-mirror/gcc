/* { dg-options "-O2 -std=c11" } */
/* { dg-require-effective-target lp64 } */

/* This tests if the instructions used for C atomic are optimised properly
   as atomic by the target code, too.  */

#include <stdatomic.h>

int load(_Atomic int *ptr)
{
        return atomic_load_explicit(ptr, memory_order_relaxed);
}

/* There should be only two machine instructions, an lwa and a blr: */
/* { dg-final { scan-assembler-times {(?n)^\s+[a-z]} 2 } } */
/* { dg-final { scan-assembler-times {\mlwa\M} 1 } } */
