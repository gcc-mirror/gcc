/* { dg-do compile { target arm*-*-* alpha*-*-* ia64*-*-* x86_64-*-* s390x-*-* powerpc*-*-* rs6000-*-* } } */
/* { dg-require-effective-target stdint_types } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-O -fdump-rtl-combine" } */

/* The test intentionally returns 1/2 instead of the obvious 0/1 to
   prevent GCC from calculating the return value with arithmetic
   instead of a comparison.  */

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
/* { dg-final { cleanup-rtl-dump "combine" } } */
