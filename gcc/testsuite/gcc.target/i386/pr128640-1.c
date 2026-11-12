/* PR target/128640 */
/* { dg-do run { target { ! ia32 } } } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O2 -mavx" } */

#include <stdarg.h>
#include "avx-check.h"

typedef float v8sf __attribute__((vector_size (32)));
typedef long long v4di __attribute__((vector_size (32)));

union u1 { v8sf f; };
union u2 { v8sf f; float x; };
struct s3 { union u1 u; };
union u4 { v4di f; };

#ifndef CHECK
#define CHECK(X, N) \
  for (int i = 0; i < N; i++) \
    if ((X)[i] != i + 1) \
      abort ()
#endif

__attribute__((noipa)) void
foo (int n, ...)
{
  va_list ap;
  va_start (ap, n);
  if (va_arg (ap, double) != 0.0)
    abort ();
  union u1 a = va_arg (ap, union u1);
  CHECK (a.f, 8);
  if (va_arg (ap, double) != 5.0)
    abort ();
  union u2 b = va_arg (ap, union u2);
  CHECK (b.f, 8);
  struct s3 c = va_arg (ap, struct s3);
  CHECK (c.u.f, 8);
  union u4 d = va_arg (ap, union u4);
  CHECK (d.f, 4);
  if (va_arg (ap, double) != 6.0)
    abort ();
  va_end (ap);
}

static void
avx_test (void)
{
  union u1 a = { { 1, 2, 3, 4, 5, 6, 7, 8 } };
  union u2 b = { { 1, 2, 3, 4, 5, 6, 7, 8 } };
  struct s3 c = { { { 1, 2, 3, 4, 5, 6, 7, 8 } } };
  union u4 d = { { 1, 2, 3, 4 } };
  foo (1, 0.0, a, 5.0, b, c, d, 6.0);
}
