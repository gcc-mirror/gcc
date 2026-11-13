/* { dg-do run } */
/* { dg-require-effective-target "riscv_zvbb_ok" } */
/* { dg-add-options "riscv_v" } */
/* { dg-add-options "riscv_zvbb" } */
/* { dg-additional-options "-std=c99 -O2" } */

#include "bswap-zvbb-perm-template.h"

#include <assert.h>

#define CHECK(NAME, VT, ETYPE, N)                                              \
  {                                                                            \
    VT x;                                                                      \
    _Pragma ("GCC novector")                                                   \
    for (int i = 0; i < N; i++)                                                \
      x[i] = (ETYPE) (0x9e3779b97f4a7c15ull * (i + 1));                        \
    VT r = perm_##NAME (x);                                                    \
    _Pragma ("GCC novector")                                                   \
    for (int i = 0; i < N; i++)                                                \
      assert (r[i] == x[idx_##NAME[i]]);                                       \
  }

int
main ()
{
  CHECK (bswap16, v16qi, uint8_t, 16)
  CHECK (bswap32, v16qi, uint8_t, 16)
  CHECK (bswap64, v16qi, uint8_t, 16)
  CHECK (reverse, v16qi, uint8_t, 16)
  CHECK (swaphi, v16qi, uint8_t, 16)
  CHECK (swappair, v8hi, uint16_t, 8)
}
