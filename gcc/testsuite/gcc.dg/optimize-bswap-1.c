/* { dg-do compile } */
/* { dg-require-effective-target stdint_types } */
/* { dg-options "-O2 -fdump-tree-bswap" } */

#include <stdint.h>

#define __const_swab32(x) ((uint32_t)(				      \
        (((uint32_t)(x) & (uint32_t)0x000000ffUL) << 24) |            \
        (((uint32_t)(x) & (uint32_t)0x0000ff00UL) <<  8) |            \
        (((uint32_t)(x) & (uint32_t)0x00ff0000UL) >>  8) |            \
        (((uint32_t)(x) & (uint32_t)0xff000000UL) >> 24)))

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

uint32_t
swap32_a (uint32_t in)
{
  return __const_swab32 (in);
}

uint64_t
swap64 (uint64_t in)
{
  return __const_swab64 (in);
}

/* The OpenSSH byte swap implementation.  */
uint32_t
swap32_b (uint32_t in)
{
  uint32_t a;

  a = (in << 16) | (in >> 16);
  a = ((a & 0x00ff00ff) << 8) | ((a & 0xff00ff00) >> 8);

  return a;
}

/* { dg-final { scan-tree-dump-times "32 bit bswap implementation found at" 2 "bswap" } } */
/* { dg-final { scan-tree-dump-times "64 bit bswap implementation found at" 1 "bswap" } } */
/* { dg-final { cleanup-tree-dump "bswap" } } */
