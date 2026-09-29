/* PR target/127650 */
/* { dg-do run } */
/* { dg-options "-O2" } */

#include <stdarg.h>

struct S { long a, b, c, d, e, f, g, h; };
typedef struct S S32 __attribute__((aligned (32)));

struct S gg;

/* The stack slot of an S32 argument is only 8-byte aligned, because the
   ABI boundary is computed from the main variant.  The va_arg access
   must not assume the typedef's 32-byte alignment, or the copy may use
   aligned vector moves on a misaligned address.  */

__attribute__((noipa)) void
f (int n, int b, int c, int d, int e, int g, ...)
{
  va_list ap;
  va_start (ap, g);
  long l = va_arg (ap, long);
  gg = va_arg (ap, S32);
  va_end (ap);
  if (l != 99 || gg.a != 11 || gg.h != 18)
    __builtin_abort ();
}

int
main (void)
{
  S32 s = { 11, 12, 13, 14, 15, 16, 17, 18 };
  f (0, 1, 2, 3, 4, 5, 99L, s);
  return 0;
}
