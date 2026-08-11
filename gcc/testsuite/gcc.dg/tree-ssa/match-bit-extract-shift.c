/* { dg-require-effective-target stdint_types } */
/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-forwprop1" } */

#include <stdint.h>

uint32_t f (uint64_t x)
{
  return (((x >> 32) & 0x1) << 16)
	  | (((x >> 24) & 0xff) << 8);
}

uint32_t f2 (uint64_t x)
{
  return (((x >> 3) & 0x3) << 16);
}

uint32_t f3 (uint64_t x)
{
  return (((x >> 61) & 0xE) << 16);
}

uint32_t f4 (uint64_t x)
{
  return (((x >> 62) & 0xE) << 16);
}

uint32_t f5 (uint64_t x)
{
  return (((x >> 63) & 0xF) << 17);
}

uint64_t f6 (uint64_t x)
{
  return (((x >> 32) & 0xFFFFFF) << 47);
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

