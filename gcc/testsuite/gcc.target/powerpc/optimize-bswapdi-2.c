/* { dg-require-effective-target stdint_types } */
/* { dg-require-effective-target lp64 } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power5" } } */
/* { dg-options "-O2 -mcpu=power5" } */

/* This is a clone of gcc-dg/optimize-bswapdi-1.c, redone to use load and stores
   to test whether lwbrx/stwbrx is generated for normal power systems.  */

#include <stdint.h>
#define __const_swab64(x) ((uint64_t)(			                        \
	(((uint64_t)(x) & (uint64_t)0x00000000000000ffULL) << 56) |             \
	(((uint64_t)(x) & (uint64_t)0x000000000000ff00ULL) << 40) |		\
	(((uint64_t)(x) & (uint64_t)0x0000000000ff0000ULL) << 24) |		\
	(((uint64_t)(x) & (uint64_t)0x00000000ff000000ULL) <<  8) |		\
	(((uint64_t)(x) & (uint64_t)0x000000ff00000000ULL) >>  8) |		\
	(((uint64_t)(x) & (uint64_t)0x0000ff0000000000ULL) >> 24) |		\
	(((uint64_t)(x) & (uint64_t)0x00ff000000000000ULL) >> 40) |		\
	(((uint64_t)(x) & (uint64_t)0xff00000000000000ULL) >> 56)))


/* This byte swap implementation is used by the Linux kernel and the
   GNU C library.  */

uint64_t
swap64_load (uint64_t *in)
{
  return __const_swab64 (*in);
}

void
swap64_store (uint64_t *out, uint64_t in)
{
  *out = __const_swab64 (in);
}

/* { dg-final { scan-assembler-times "lwbrx" 2 } } */
/* { dg-final { scan-assembler-times "stwbrx" 2 } } */
