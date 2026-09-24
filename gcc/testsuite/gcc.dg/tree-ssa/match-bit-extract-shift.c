/* { dg-require-effective-target stdint_types } */
/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-forwprop1" } */

#include <stdint.h>

uint32_t f (uint64_t x)
{
  return (((x >> 32) & 0x1) << 16L)
	  | (((x >> 24UL) & 0xff) << 8);
}

uint32_t f2 (uint64_t x)
{
  return (((x >> 3) & 0x3) << 16LL);
}

uint32_t f3 (uint64_t x)
{
  return (((x >> 61ULL) & 0xE) << 16);
}

uint32_t f4 (uint64_t x)
{
  return (((x >> 62U) & 0xE) << 16UL);
}

uint32_t f5 (uint64_t x)
{
  return (((x >> 63L) & 0xF) << 17U);
}

uint64_t f6 (uint64_t x)
{
  return (((x >> 32U) & 0xFFFFFF) << 47ULL);
}

/* { dg-final { scan-tree-dump-not "<< 16" forwprop1 } } */
/* { dg-final { scan-tree-dump-not ">> 32" forwprop1 } } */
/* { dg-final { scan-tree-dump-not ">> 24" forwprop1 } } */
/* { dg-final { scan-tree-dump-not ">> 3" forwprop1 } } */
/* { dg-final { scan-tree-dump-not ">> 61" forwprop1 } } */
/* { dg-final { scan-tree-dump-not ">> 62" forwprop1 } } */
/* { dg-final { scan-tree-dump-not "<< 47" forwprop1 } } */
/* { dg-final { scan-tree-dump-not "& 65536" forwprop1 } } */
/* { dg-final { scan-tree-dump-not "& 65535" forwprop1 } } */
/* { dg-final { scan-tree-dump-not "& 917504" forwprop1 } } */

/* { dg-final { scan-tree-dump-times ">> 16" 1 forwprop1 } } */
/* { dg-final { scan-tree-dump-times ">> 45" 1 forwprop1 } } */
/* { dg-final { scan-tree-dump-times ">> 46" 1 forwprop1 } } */
/* { dg-final { scan-tree-dump-times "<< 13" 1 forwprop1 } } */
/* { dg-final { scan-tree-dump-times ">> 63" 1 forwprop1 } } */
/* { dg-final { scan-tree-dump-times "<< 17" 1 forwprop1 } } */
/* { dg-final { scan-tree-dump-times "<< 15" 1 forwprop1 } } */
/* { dg-final { scan-tree-dump-times "& 130816" 1 forwprop1 } } */
/* { dg-final { scan-tree-dump-times "& 196608" 1 forwprop1 } } */
/* { dg-final { scan-tree-dump-times "& 393216" 1 forwprop1 } } */
/* { dg-final { scan-tree-dump-times "& 131072" 1 forwprop1 } } */
/* { dg-final { scan-tree-dump-times "& 18446603336221196288" 1 forwprop1 } } */

