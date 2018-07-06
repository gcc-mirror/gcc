/* { dg-do compile } */
/* { dg-require-effective-target bswap } */
/* { dg-require-effective-target stdint_types } */
/* { dg-options "-O2 -fdump-tree-bswap" } */
/* { dg-additional-options "-mzarch" { target s390*-*-* } } */

#include <stdint.h>

/* A variant via unsigned short.  */

uint64_t
swap64_c (uint64_t x)
{
  uint16_t a0 = x >> 48;
  uint16_t a1 = x >> 32;
  uint16_t a2 = x >> 16;
  uint16_t a3 = x;

  return ((uint64_t) (((a0 >> 8) & 0xff) | ((a0 << 8) & 0xff00)))
	| ((uint64_t) (((a1 >> 8) & 0xff) | ((a1 << 8) & 0xff00)) << 16)
	| ((uint64_t) (((a2 >> 8) & 0xff) | ((a2 << 8) & 0xff00)) << 32)
	| ((uint64_t) (((a3 >> 8) & 0xff) | ((a3 << 8) & 0xff00)) << 48);
}


/* { dg-final { scan-tree-dump-times "64 bit bswap implementation found at" 1 "bswap" } } */
