/* { dg-do compile { target { { i?86-*-* x86_64-*-* } && { ! ia32 } } } } */
/* { dg-options "-fdump-rtl-ce1 -O2" } */

typedef unsigned long long uint64_t;

static uint64_t umulh(uint64_t a, uint64_t b)
{
  return (unsigned __int128)a*b >> 64;
}

uint64_t f(uint64_t a, uint64_t b, int c)
{
  if (c)
    a = umulh(a, (b-umulh(a,b))<<44) << 1;
  return a;
}

/* { dg-final { scan-rtl-dump "0 true changes made" "ce1" } } */
/* { dg-final { scan-assembler-not "cmov" } } */
