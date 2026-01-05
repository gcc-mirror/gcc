/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-times "csel\t" 2 } } */

#include <stdint.h>
#include <stddef.h>

// Branchy: compiler can emit a short compare+branch.
size_t branchy(size_t cur, size_t dist, size_t fallback, const uint8_t *src, uint8_t *dst, size_t len) {
    if (dist < cur && dist < fallback) {
        const uint8_t *p = src - dist;
        for (size_t i = 0; i < len; ++i)
	  dst[i] = p[i];
        return dist;
    } else {
        const uint8_t *p = src - fallback;
        for (size_t i = 0; i < len; ++i)
	  dst[i] = p[i];
        return fallback;
    }
}

// CSEL-heavy: chain ternaries so both paths stay live; compilers emit cmps + csel.
size_t selecty(size_t cur, size_t dist, size_t fallback, const uint8_t *src, uint8_t *dst, size_t len) {
    // Compute both candidates unconditionally, then pick with ternaries.
    size_t candA_off = dist;
    size_t candB_off = fallback;
    const uint8_t *candA = src - candA_off;
    const uint8_t *candB = src - candB_off;

    size_t useA = (dist < cur && dist < fallback);
    const uint8_t *p = useA ? candA : candB;
    size_t chosen = useA ? candA_off : candB_off;

    for (size_t i = 0; i < len; ++i)
      dst[i] = p[i];
    return chosen;
}
