/* PR target/108599 */
/* { dg-do run { target avx2 } } */
/* { dg-options "-O2 -mavx2 -mtune=skylake-avx512" } */

#include "avx2-check.h"

struct S { unsigned long long a, b, c, d; };

__attribute__((noipa)) void
foo (unsigned long long x, unsigned long long y,
     unsigned long long z, unsigned long long w, const struct S s)
{
  if (s.a != x || s.b != y || s.c != z || s.d != w)
    abort ();
}

typedef unsigned long long V __attribute__((may_alias, vector_size (4 * sizeof (unsigned long long))));

static void
avx2_test (void)
{
  {
    struct S s;
    *(V *)&s = (V) { 16, 0, 0, 0 };
    foo (16, 0, 0, 0, s);
  }
  {
    struct S s;
    *(V *)&s = (V) { 16, 16, 0, 0 };
    foo (16, 16, 0, 0, s);
  }
}
