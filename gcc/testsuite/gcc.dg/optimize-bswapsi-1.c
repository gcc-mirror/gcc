/* { dg-do compile } */
/* { dg-require-effective-target bswap } */
/* { dg-require-effective-target stdint_types } */
/* { dg-options "-O2 -fdump-tree-bswap" } */
/* { dg-additional-options "-march=z900" { target s390*-*-* } } */

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

/* This variant comes from gcc.target/sh/pr53568-1.c.  It requires to track
   which bytes have an unpredictable value (eg. due to sign extension) to
   make sure that the final expression have only well defined byte values.  */

SItype
swap32_d (SItype in)
{
  /* 1x swap.w
     2x swap.b  */
  return (((in >> 0) & 0xFF) << 24)
	 | (((in >> 8) & 0xFF) << 16)
	 | (((in >> 16) & 0xFF) << 8)
	 | (((in >> 24) & 0xFF) << 0);
}

/* This variant is adapted from swap32_d above.  It detects missing cast of
   MARKER_BYTE_UNKNOWN to uint64_t for the CASE_CONVERT case for host
   architecture where a left shift with too big an operand mask its high
   bits.  */

SItype
swap32_e (SItype in)
{
  return (((in >> 0) & 0xFF) << 24)
	 | (((in >> 8) & 0xFF) << 16)
	 | (((((int64_t) in) & 0xFF0000FF0000) >> 16) << 8)
	 | (((in >> 24) & 0xFF) << 0);
}

/* This variant comes from PR63259.  It compiles to a gimple sequence that ends
   with a rotation instead of a bitwise OR.  */

unsigned
swap32_f (unsigned in)
{
  in = ((in & 0xff00ff00) >>  8) | ((in & 0x00ff00ff) <<  8);
  in = ((in & 0xffff0000) >> 16) | ((in & 0x0000ffff) << 16);
  return in;
}

/* { dg-final { scan-tree-dump-times "32 bit bswap implementation found at" 6 "bswap" } } */
