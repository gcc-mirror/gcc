/* { dg-do compile { target arm*-*-* alpha*-*-* ia64*-*-* i?86-*-* x86_64-*-* s390x-*-* powerpc*-*-* rs6000-*-* } } */
/* { dg-require-effective-target stdint_types } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-O -fdump-rtl-combine" } */

/* The param setting prevents the return value from being
   calculated with arithmetic instead of doing a compare.  */
/* { dg-additional-options "--param logical-op-non-short-circuit=0" } */

#include <stdint.h>

#define BS(X) __builtin_bswap64(X)

int foo1 (uint64_t a)
{
  if (BS (a) == 0xA00000000)
    return 1;
  return 0;
}

int foo2 (uint64_t a)
{
  if (BS (a) != 0xA00000000)
    return 1;
  return 0;
}

int foo3 (uint64_t a, uint64_t b)
{
  if (BS (a) == BS (b))
    return 1;
  return 0;
}

int foo4 (uint64_t a, uint64_t b)
{
  if (BS (a) != BS (b))
    return 1;
  return 0;
}

/* { dg-final { scan-rtl-dump-not "bswapdi" "combine" } } */
