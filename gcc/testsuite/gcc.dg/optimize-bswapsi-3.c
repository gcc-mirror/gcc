/* { dg-do compile } */
/* { dg-require-effective-target bswap } */
/* { dg-require-effective-target stdint_types } */
/* { dg-options "-O2 -fdump-tree-bswap" } */
/* { dg-additional-options "-march=z900" { target s390-*-* } } */

typedef int SItype __attribute__ ((mode (SI)));
typedef int DItype __attribute__ ((mode (DI)));

/* This variant comes from optimize-bswapsi-1.c swap32_d.  It detects a missing
   cast of MARKER_BYTE_UNKNOWN to uint64_t for the CASE_CONVERT case for host
   architecture where a left shift with too big an operand gives zero.  */

SItype
swap32 (SItype in)
{
  return (((in >> 0) & 0xFF) << 24)
	 | (((in >> 8) & 0xFF) << 16)
	 | (((((DItype) in) & 0xFF00FF0000llu) >> 16) << 8)
	 | (((in >> 24) & 0xFF) << 0);
}

/* { dg-final { scan-tree-dump-not "32 bit bswap implementation found at" "bswap" } } */
