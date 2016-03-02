/* PR target/65368 */
/* { dg-do run { target bmi2 } } */
/* { dg-options "-O2 -mbmi2" } */

#include <x86intrin.h>
#include "bmi2-check.h"

unsigned int a;
unsigned long long b;

#define A __attribute__((noinline, noclone))

A unsigned int f1 (void) { return _bzhi_u32 (a, 0); }
A unsigned int f2 (unsigned int x) { return _bzhi_u32 (x, 0); }
A unsigned int f3 (void) { return _bzhi_u32 (a, 5); }
A unsigned int f4 (unsigned int x) { return _bzhi_u32 (x, 5); }
A unsigned int f5 (void) { return _bzhi_u32 (a, 31); }
A unsigned int f6 (unsigned int x) { return _bzhi_u32 (x, 31); }
A unsigned int f7 (void) { return _bzhi_u32 (a, 32); }
A unsigned int f8 (unsigned int x) { return _bzhi_u32 (x, 32); }
A unsigned int f9 (void) { return _bzhi_u32 (a, 37); }
A unsigned int f10 (unsigned int x) { return _bzhi_u32 (x, 37); }
A unsigned int f11 (void) { return _bzhi_u32 (a, 257); }
A unsigned int f12 (unsigned int x) { return _bzhi_u32 (x, 257); }
A unsigned int f13 (void) { return _bzhi_u32 (a, 289); }
A unsigned int f14 (unsigned int x) { return _bzhi_u32 (x, 289); }
#ifdef __x86_64__
A unsigned long long f21 (void) { return _bzhi_u64 (b, 0); }
A unsigned long long f22 (unsigned long long x) { return _bzhi_u64 (x, 0); }
A unsigned long long f23 (void) { return _bzhi_u64 (b, 5); }
A unsigned long long f24 (unsigned long long x) { return _bzhi_u64 (x, 5); }
A unsigned long long f25 (void) { return _bzhi_u64 (b, 63); }
A unsigned long long f26 (unsigned long long x) { return _bzhi_u64 (x, 63); }
A unsigned long long f27 (void) { return _bzhi_u64 (b, 64); }
A unsigned long long f28 (unsigned long long x) { return _bzhi_u64 (x, 64); }
A unsigned long long f29 (void) { return _bzhi_u64 (b, 69); }
A unsigned long long f30 (unsigned long long x) { return _bzhi_u64 (x, 69); }
A unsigned long long f31 (void) { return _bzhi_u64 (b, 257); }
A unsigned long long f32 (unsigned long long x) { return _bzhi_u64 (x, 257); }
A unsigned long long f33 (void) { return _bzhi_u64 (b, 321); }
A unsigned long long f34 (unsigned long long x) { return _bzhi_u64 (x, 321); }
#endif

static void
bmi2_test ()
{
  a = -1U;
  b = -1ULL;
  if (f1 () != 0 || f2 (-1U) != 0
      || f3 () != 0x1f || f4 (-1U) != 0x1f
      || f5 () != 0x7fffffffU || f6 (-1U) != 0x7fffffffU
      || f7 () != -1U || f8 (-1U) != -1U
      || f9 () != -1U || f10 (-1U) != -1U
      || f11 () != 1 || f12 (-1U) != 1
      || f13 () != -1U || f14 (-1U) != -1U)
    abort ();
#ifdef __x86_64__
  if (f21 () != 0 || f22 (-1ULL) != 0
      || f23 () != 0x1f || f24 (-1ULL) != 0x1f
      || f25 () != 0x7fffffffffffffffULL || f26 (-1ULL) != 0x7fffffffffffffffULL
      || f27 () != -1ULL || f28 (-1ULL) != -1ULL
      || f29 () != -1ULL || f30 (-1ULL) != -1ULL
      || f31 () != 1 || f32 (-1ULL) != 1
      || f33 () != -1ULL || f34 (-1ULL) != -1ULL)
    abort ();
#endif
}
