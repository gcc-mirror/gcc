/* { dg-do compile } */
/* { dg-options "-O2 -mbmi2 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-not "link_error" "optimized" } } */

#include <x86intrin.h>

extern void link_error (void);
unsigned int a;
unsigned long long b;

static inline unsigned int f1 (void) { return _bzhi_u32 (a, 0); }
static inline unsigned int f2 (unsigned int x) { return _bzhi_u32 (x, 0); }
static inline unsigned int f3 (void) { return _bzhi_u32 (a, 5); }
static inline unsigned int f4 (unsigned int x) { return _bzhi_u32 (x, 5); }
static inline unsigned int f5 (void) { return _bzhi_u32 (a, 31); }
static inline unsigned int f6 (unsigned int x) { return _bzhi_u32 (x, 31); }
static inline unsigned int f7 (void) { return _bzhi_u32 (a, 32); }
static inline unsigned int f8 (unsigned int x) { return _bzhi_u32 (x, 32); }
static inline unsigned int f9 (void) { return _bzhi_u32 (a, 37); }
static inline unsigned int f10 (unsigned int x) { return _bzhi_u32 (x, 37); }
static inline unsigned int f11 (void) { return _bzhi_u32 (a, 257); }
static inline unsigned int f12 (unsigned int x) { return _bzhi_u32 (x, 257); }
static inline unsigned int f13 (void) { return _bzhi_u32 (a, 289); }
static inline unsigned int f14 (unsigned int x) { return _bzhi_u32 (x, 289); }
#ifdef __x86_64__
static inline unsigned long long f21 (void) { return _bzhi_u64 (b, 0); }
static inline unsigned long long f22 (unsigned long long x) { return _bzhi_u64 (x, 0); }
static inline unsigned long long f23 (void) { return _bzhi_u64 (b, 5); }
static inline unsigned long long f24 (unsigned long long x) { return _bzhi_u64 (x, 5); }
static inline unsigned long long f25 (void) { return _bzhi_u64 (b, 63); }
static inline unsigned long long f26 (unsigned long long x) { return _bzhi_u64 (x, 63); }
static inline unsigned long long f27 (void) { return _bzhi_u64 (b, 64); }
static inline unsigned long long f28 (unsigned long long x) { return _bzhi_u64 (x, 64); }
static inline unsigned long long f29 (void) { return _bzhi_u64 (b, 69); }
static inline unsigned long long f30 (unsigned long long x) { return _bzhi_u64 (x, 69); }
static inline unsigned long long f31 (void) { return _bzhi_u64 (b, 257); }
static inline unsigned long long f32 (unsigned long long x) { return _bzhi_u64 (x, 257); }
static inline unsigned long long f33 (void) { return _bzhi_u64 (b, 321); }
static inline unsigned long long f34 (unsigned long long x) { return _bzhi_u64 (x, 321); }
#endif

unsigned int c;
unsigned long long d;

int
main ()
{
  asm volatile ("" : : "g" (&c), "g" (&d) : "memory");
  a = -1U;
  b = -1ULL;
  if (f1 () != 0 || f2 (-1U) != 0
      || f3 () != 0x1f || f4 (-1U) != 0x1f
      || f5 () != 0x7fffffffU || f6 (-1U) != 0x7fffffffU
      || f7 () != -1U || f8 (-1U) != -1U
      || f9 () != -1U || f10 (-1U) != -1U
      || f11 () != 1 || f12 (-1U) != 1
      || f13 () != -1U || f14 (-1U) != -1U)
    link_error ();
  if (_bzhi_u32 (c, 32) != c
      || _bzhi_u32 (c, 64) != c
      || _bzhi_u32 (c, 255) != c)
    link_error ();
#ifdef __x86_64__
  if (f21 () != 0 || f22 (-1ULL) != 0
      || f23 () != 0x1f || f24 (-1ULL) != 0x1f
      || f25 () != 0x7fffffffffffffffULL || f26 (-1ULL) != 0x7fffffffffffffffULL
      || f27 () != -1ULL || f28 (-1ULL) != -1ULL
      || f29 () != -1ULL || f30 (-1ULL) != -1ULL
      || f31 () != 1 || f32 (-1ULL) != 1
      || f33 () != -1ULL || f34 (-1ULL) != -1ULL)
    link_error ();
  if (_bzhi_u64 (d, 64) != d
      || _bzhi_u64 (d, 255) != d)
    link_error ();
#endif
  return 0;
}
