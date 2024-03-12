/* { dg-do compile { target has_arch_ppc64 } } */
/* { dg-options "-O2" } */

#define C1 0x2351847027482577ULL
#define C2 0x2351847027482578ULL

void __attribute__ ((noinline)) foo (long long *a)
{
  *a++ = C1;
  *a++ = C2;
}

void __attribute__ ((noinline)) foo1 (long long *a, long long b)
{
  *a++ = C1;
  if (b)
    *a++ = C2;
}

/* { dg-final { scan-assembler-times {\maddi\M} 2 } } */
