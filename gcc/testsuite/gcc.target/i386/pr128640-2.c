/* PR target/128640 */
/* { dg-do run { target { ! ia32 } } } */
/* { dg-require-effective-target avx512f } */
/* { dg-options "-O2 -mavx512f" } */

#include <stdarg.h>
#include "avx512f-check.h"

typedef float v16sf __attribute__((vector_size (64)));
typedef long long v8di __attribute__((vector_size (64)));

union u1 { v16sf f; };
union u2 { v16sf f; float x; };
struct s3 { union u1 u; };
union u4 { v8di f; };

#define CHECK(X, N) \
  for (int i = 0; i < N; i++) \
    if ((X)[i] != i + 1) \
      abort ()

__attribute__((noipa)) void
foo (int n, ...)
{
  va_list ap;
  va_start (ap, n);
  if (va_arg (ap, double) != 0.0)
    abort ();
  union u1 a = va_arg (ap, union u1);
  CHECK (a.f, 16);
  if (va_arg (ap, double) != 5.0)
    abort ();
  union u2 b = va_arg (ap, union u2);
  CHECK (b.f, 16);
  struct s3 c = va_arg (ap, struct s3);
  CHECK (c.u.f, 16);
  union u4 d = va_arg (ap, union u4);
  CHECK (d.f, 8);
  if (va_arg (ap, double) != 6.0)
    abort ();
  va_end (ap);
}

static void
avx512f_test (void)
{
  union u1 a = { { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16 } };
  union u2 b = { { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16 } };
  struct s3 c = { { { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16 } } };
  union u4 d = { { 1, 2, 3, 4, 5, 6, 7, 8 } };
  foo (1, 0.0, a, 5.0, b, c, d, 6.0);
}
