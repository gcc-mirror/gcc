/* { dg-do compile { target arm*-*-* alpha*-*-* ia64*-*-* i?86-*-* x86_64-*-* s390x-*-* powerpc*-*-* rs6000-*-* } } */
/* { dg-require-effective-target stdint_types } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-O2 -fdump-rtl-combine" } */

#include <stdint.h>

#define BS(X) __builtin_bswap64(X)

uint64_t foo1 (uint64_t a)
{
  return BS (~ BS (a));
}

uint64_t foo2 (uint64_t a)
{
  return BS (BS (a) & 0xA00000000);
}

uint64_t foo3 (uint64_t a)
{
  return BS (BS (a) | 0xA00000000);
}

uint64_t foo4 (uint64_t a)
{
  return BS (BS (a) ^ 0xA00000000);
}

uint64_t foo5 (uint64_t a, uint64_t b)
{
  return BS (BS (a) & BS (b));
}

uint64_t foo6 (uint64_t a, uint64_t b)
{
  return BS (BS (a) | BS (b));
}

uint64_t foo7 (uint64_t a, uint64_t b)
{
  return BS (BS (a) ^ BS (b));
}

/* { dg-final { scan-rtl-dump-not "bswapdi" "combine" } } */
