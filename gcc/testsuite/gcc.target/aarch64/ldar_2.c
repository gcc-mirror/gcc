/* Test that the zero-extending patterns for LDAR are used.  */
/* { dg-do compile } */
/* { dg-options "-O2" } */

#include <stdint.h>

uint8_t v_uint8_t;
uint16_t v_uint16_t;
uint32_t v_uint32_t;
uint64_t v_uint64_t;

#define FUNC(FROM, TO)						\
TO								\
load_##FROM##_ext_##TO (void)					\
{								\
  return __atomic_load_n (&v_##FROM, __ATOMIC_ACQUIRE);		\
}

FUNC (uint8_t, uint16_t)
FUNC (uint8_t, uint32_t)
FUNC (uint8_t, uint64_t)
FUNC (uint16_t, uint32_t)
FUNC (uint16_t, uint64_t)
FUNC (uint32_t, uint64_t)

/* { dg-final { scan-assembler-not {and\tw[0-9+], w[0-9]+, 255} } } */
/* { dg-final { scan-assembler-not {uxtw\tx[0-9+], w[0-9]+} } } */
