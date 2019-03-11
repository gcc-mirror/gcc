/* { dg-require-effective-target stdint_types } */
/* { dg-options "-O2 -mdejagnu-cpu=power5" } */

#include <stdint.h>

/* This is a clone of gcc-dg/optimize-bswapsi-1.c, redone to use load and stores
   to test whether lwbrx/stwbrx is generated for normal power systems.  */

#define __const_swab32(x) ((uint32_t)(				      \
        (((uint32_t)(x) & (uint32_t)0x000000ffUL) << 24) |            \
        (((uint32_t)(x) & (uint32_t)0x0000ff00UL) <<  8) |            \
        (((uint32_t)(x) & (uint32_t)0x00ff0000UL) >>  8) |            \
        (((uint32_t)(x) & (uint32_t)0xff000000UL) >> 24)))

/* This byte swap implementation is used by the Linux kernel and the
   GNU C library.  */

uint32_t
swap32_a_load (uint32_t *in)
{
  return __const_swab32 (*in);
}

/* The OpenSSH byte swap implementation.  */
uint32_t
swap32_b_load (uint32_t *in)
{
  uint32_t a;

  a = (*in << 16) | (*in >> 16);
  a = ((a & 0x00ff00ff) << 8) | ((a & 0xff00ff00) >> 8);

  return a;
}

void
swap32_a_store (uint32_t *out, uint32_t in)
{
  *out = __const_swab32 (in);
}

/* The OpenSSH byte swap implementation.  */
void
swap32_b_store (uint32_t *out, uint32_t in)
{
  uint32_t a;

  a = (in << 16) | (in >> 16);
  a = ((a & 0x00ff00ff) << 8) | ((a & 0xff00ff00) >> 8);

  *out = a;
}

/* { dg-final { scan-assembler-times "lwbrx" 2 } } */
/* { dg-final { scan-assembler-times "stwbrx" 2 } } */
