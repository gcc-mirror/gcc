/* { dg-do compile } */
/* { dg-require-effective-target bswap64 } */
/* { dg-require-effective-target stdint_types } */
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

/* This variant is currently used by libgcc.  The difference is that
   the bswap source and destination have a signed integer type which
   requires a slightly higher search depth in order to dive through
   the cast as well.  */

typedef int DItype __attribute__ ((mode (DI)));
DItype
swap64_b (DItype u)
{
  return ((((u) & 0xff00000000000000ull) >> 56)
	  | (((u) & 0x00ff000000000000ull) >> 40)
	  | (((u) & 0x0000ff0000000000ull) >> 24)
	  | (((u) & 0x000000ff00000000ull) >>  8)
	  | (((u) & 0x00000000ff000000ull) <<  8)
	  | (((u) & 0x0000000000ff0000ull) << 24)
	  | (((u) & 0x000000000000ff00ull) << 40)
	  | (((u) & 0x00000000000000ffull) << 56));
}

/* The OpenSSL variant.  */

uint64_t
swap64_c (uint64_t x)
{
  uint32_t a = x >> 32;
  uint32_t b = (uint32_t) x;
  return ((uint64_t) ((((((b)) >> (8)) | (((b)) << (32 - (8)))) & 0xff00ff00L)
		      | (((((b)) << (8)) | (((b)) >> (32 - (8)))) & 0x00ff00ffL)) << 32)
          | (uint64_t) ((((((a)) >> (8)) | (((a)) << (32 - (8)))) & 0xff00ff00L)
			| (((((a)) << (8)) | (((a)) >> (32 - (8)))) & 0x00ff00ffL));
}


/* { dg-final { scan-tree-dump-times "64 bit bswap implementation found at" 3 "bswap" } } */
/* { dg-final { cleanup-tree-dump "bswap" } } */
