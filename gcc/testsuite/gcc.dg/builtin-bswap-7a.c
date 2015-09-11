/* { dg-do compile { target arm*-*-* alpha*-*-* ia64*-*-* i?86-*-* x86_64-*-* s390x-*-* powerpc*-*-* rs6000-*-* } } */
/* { dg-require-effective-target stdint_types } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-O2 -fdump-rtl-combine" } */

/* The test is similiar to builtin-bswap-7.c but returns 1/2 instead
   of 0/1 to prevent GCC from calculating the return value with
   arithmetic instead of a comparison.  This requires the optimization
   level to be bumped up to -O2 at least for x86_64.  */

#include <stdint.h>

#define BS(X) __builtin_bswap64(X)

int foo1 (uint64_t a)
{
  if (BS (a) == 0xA00000000)
    return 1;
  return 2;
}

int foo2 (uint64_t a)
{
  if (BS (a) != 0xA00000000)
    return 1;
  return 2;
}

int foo3 (uint64_t a, uint64_t b)
{
  if (BS (a) == BS (b))
    return 1;
  return 2;
}

int foo4 (uint64_t a, uint64_t b)
{
  if (BS (a) != BS (b))
    return 1;
  return 2;
}

/* { dg-final { scan-rtl-dump-not "bswapdi" "combine" } } */
