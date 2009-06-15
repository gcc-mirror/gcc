/* { dg-do compile { target alpha*-*-* ia64*-*-* x86_64-*-* s390x-*-* } } */
/* { dg-require-effective-target stdint_types } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-O2 -fdump-tree-bswap" } */

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
swap64 (uint64_t in)
{
  return __const_swab64 (in);
}

/* { dg-final { scan-tree-dump-times "64 bit bswap implementation found at" 1 "bswap" } } */
/* { dg-final { cleanup-tree-dump "bswap" } } */
