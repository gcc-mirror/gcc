/* { dg-do compile } */
/* { dg-options "-march=rv64gc -mabi=lp64" } */
#include <stdint-gcc.h>

void foo(uint32_t *p) {
    uintptr_t x = *(uintptr_t *)p;
    uint32_t e = !p ? 0 : (uintptr_t)p >> 1;
    uint32_t d = (uintptr_t)x;
    __atomic_compare_exchange(p, &e, &d, 0, __ATOMIC_RELAXED, __ATOMIC_RELAXED);
}

/* { dg-final { scan-assembler-bound {sext.w\t} >= 1 } } */
