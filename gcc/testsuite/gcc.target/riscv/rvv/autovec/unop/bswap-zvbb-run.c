/* { dg-do run } */
/* { dg-require-effective-target "riscv_zvbb_ok" } */
/* { dg-add-options "riscv_v" } */
/* { dg-add-options "riscv_zvbb" } */
/* { dg-additional-options "-std=c99 -fno-vect-cost-model" } */

#include "bswap-zvbb-template.h"

#include <assert.h>

#define SZ 255

#define RUN(TYPE, BITS)                                                        \
  {                                                                            \
    TYPE dst[SZ];                                                              \
    TYPE a[SZ];                                                                \
    for (int i = 0; i < SZ; i++)                                               \
      {                                                                        \
	dst[i] = 0;                                                            \
	a[i] = (TYPE) (0x9e3779b97f4a7c15ull * (i + 1));                       \
      }                                                                        \
    vbswap_##TYPE (dst, a, SZ);                                                \
    for (int i = 0; i < SZ; i++)                                               \
      {                                                                        \
	TYPE ref = 0;                                                          \
	_Pragma ("GCC novector")                                               \
	for (int j = 0; j < BITS / 8; j++)                                     \
	  ref |= (TYPE) ((a[i] >> (8 * j)) & 0xff) << (BITS - 8 - 8 * j);      \
	assert (dst[i] == ref);                                                \
      }                                                                        \
  }

int
main ()
{
  RUN (uint16_t, 16)
  RUN (uint32_t, 32)
  RUN (uint64_t, 64)
}
