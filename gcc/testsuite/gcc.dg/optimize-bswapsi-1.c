/* { dg-do compile { target alpha*-*-* i?86-*-* powerpc*-*-* rs6000-*-* x86_64-*-* s390*-*-* } } */
/* { dg-require-effective-target stdint_types } */
/* { dg-options "-O2 -fdump-tree-bswap" } */

#include <stdint.h>

#define __const_swab32(x) ((uint32_t)(				      \
        (((uint32_t)(x) & (uint32_t)0x000000ffUL) << 24) |            \
        (((uint32_t)(x) & (uint32_t)0x0000ff00UL) <<  8) |            \
        (((uint32_t)(x) & (uint32_t)0x00ff0000UL) >>  8) |            \
        (((uint32_t)(x) & (uint32_t)0xff000000UL) >> 24)))

/* This byte swap implementation is used by the Linux kernel and the
   GNU C library.  */

uint32_t
swap32_a (uint32_t in)
{
  return __const_swab32 (in);
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

/* This variant is currently used by libgcc.  The difference is that
   the bswap source and destination have a signed integer type which
   requires a slightly higher search depth in order to dive through
   the cast as well.  */

typedef int SItype __attribute__ ((mode (SI)));

SItype
swap32_c (SItype u)
{
  return ((((u) & 0xff000000) >> 24)
	  | (((u) & 0x00ff0000) >>  8)
	  | (((u) & 0x0000ff00) <<  8)
	  | (((u) & 0x000000ff) << 24));
}

/* { dg-final { scan-tree-dump-times "32 bit bswap implementation found at" 3 "bswap" } } */
/* { dg-final { cleanup-tree-dump "bswap" } } */
