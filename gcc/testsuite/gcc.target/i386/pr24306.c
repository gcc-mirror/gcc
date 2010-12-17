/* { dg-do run } */
/* { dg-options "-msse" } */
/* { dg-require-effective-target sse } */

#include "sse-check.h"

extern void abort(void);
typedef int __attribute__ ((vector_size (16))) foo_t;

struct s
{
  foo_t f[0];
} s1;

void
check (int x, ...) __attribute__((noinline));
void
check (int x, ...)
{
  int y;
  __builtin_va_list ap;

  __builtin_va_start (ap, x);
  __builtin_va_arg (ap, struct s);
  y = __builtin_va_arg (ap, int);

  if (y != 7)
    abort ();
}

static void
sse_test (void)
{
  check (3, s1, 7);
}
