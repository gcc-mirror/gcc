/* { dg-do compile } */
/* { dg-options "-O2 -march=rv32gcv_zilsd -mabi=ilp32d -mrvv-vector-bits=scalable -fno-vect-cost-model" } */
/* { dg-additional-options "-mzilsd-strict-align" } */

#include <stdint-gcc.h>

/* On RV32 an int64_t argument passed after seven pointer arguments is
   split: the low word lands in the last available argument register (a7)
   and the high word on the incoming stack.  When the vectoriser later
   spills the low word of n to a 4-byte-aligned stack slot, the backend
   must not reload that value with a single misaligned zilsd "ld" under
   -mzilsd-strict-align; it must split the reload into two 32-bit loads
   instead, otherwise the access faults on targets that trap on misaligned
   64-bit loads.  */

void __attribute__ ((noinline, noclone))
f6 (uint64_t *__restrict a, uint64_t *__restrict b, uint64_t *__restrict c,
    uint64_t *__restrict d, uint64_t *__restrict e, uint64_t *__restrict f,
    uint64_t *__restrict g, int64_t n)
{
  for (int64_t i = 0; i < n; ++i)
    {
      a[i] = g[i * 6];
      b[i] = g[i * 6 + 1];
      c[i] = g[i * 6 + 2];
      d[i] = g[i * 6 + 3];
      e[i] = g[i * 6 + 4];
      f[i] = g[i * 6 + 5];
    }
}

/* The spilled low word of n sits at a 4-byte-aligned stack offset (76);
   it must be reloaded with "lw", never with a single misaligned zilsd
   "ld".  The vector loop counter, in contrast, lives at an 8-byte-aligned
   offset (8) and may legitimately use ld/sd.  */
/* { dg-final { scan-assembler-not {ld\t[a-z0-9]+,76\(sp\)} } } */
/* { dg-final { scan-assembler-not {sd\t[a-z0-9]+,76\(sp\)} } } */
