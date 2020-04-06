/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O -mavx" } */

#include "avx-check.h"

typedef char __attribute__ ((__vector_size__ (8))) v8qi;
typedef short __attribute__ ((__vector_size__ (8))) v4hi;
typedef int __attribute__ ((__vector_size__ (8))) v2si;
typedef long long __attribute__ ((__vector_size__ (8))) v1di;
typedef unsigned long long u64;
u64 k, c;

v8qi g, h, p, q;
v4hi d, e, f, l, n, o;
v2si j;

u64
foo (v4hi r)
{
  v8qi s;
  f = (v4hi) j;
  e = __builtin_ia32_psrlwi ((v4hi) k, c);
  s = __builtin_ia32_pavgb (h, h);
  n = __builtin_ia32_pabsw (f);
  o = __builtin_ia32_psubusw (n, l);
  p = __builtin_ia32_packsswb (r, o);
  q = __builtin_ia32_pshufb (p, s);
  g = __builtin_ia32_punpcklbw (q, (v8qi) r);
  d = r;
  return (u64) g + (u64) h + (u64) j;
}

static void
avx_test (void)
{
  u64 x = foo ((v4hi) { 5 });
  if (x != 0x0005000500050505)
    __builtin_abort ();
}
