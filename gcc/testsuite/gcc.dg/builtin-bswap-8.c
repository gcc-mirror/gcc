/* { dg-do compile { target arm*-*-* alpha*-*-* i?86-*-* powerpc*-*-* rs6000-*-* x86_64-*-* s390*-*-* } } */
/* { dg-require-effective-target stdint_types } */
/* { dg-options "-O2 -fdump-rtl-combine" } */
/* { dg-options "-O2 -fdump-rtl-combine -march=z900" { target s390-*-* } } */

#include <stdint.h>

#define BS(X) __builtin_bswap32(X)

uint32_t foo1 (uint32_t a)
{
  return BS (~ BS (a));
}

uint32_t foo2 (uint32_t a)
{
  return BS (BS (a) & 0xA0000);
}

uint32_t foo3 (uint32_t a)
{
  return BS (BS (a) | 0xA0000);
}

uint32_t foo4 (uint32_t a)
{
  return BS (BS (a) ^ 0xA0000);
}

uint32_t foo5 (uint32_t a, uint32_t b)
{
  return BS (BS (a) & BS (b));
}

uint32_t foo6 (uint32_t a, uint32_t b)
{
  return BS (BS (a) | BS (b));
}

uint32_t foo7 (uint32_t a, uint32_t b)
{
  return BS (BS (a) ^ BS (b));
}

/* { dg-final { scan-rtl-dump-not "bswapsi" "combine" } } */
