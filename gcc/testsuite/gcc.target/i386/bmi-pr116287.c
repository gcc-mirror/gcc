/* PR target/116287 */
/* { dg-do run { target bmi } } */
/* { dg-options "-O2 -mbmi" } */

#include <x86intrin.h>

#include "bmi-check.h"

static void
bmi_test ()
{
  unsigned int a = 0;
  if (__builtin_ia32_bextr_u32 (a++, 0) != 0)
    abort ();
  if (__builtin_ia32_bextr_u32 (a++, 0x120) != 0)
    abort ();
  if (a != 2)
    abort ();
#ifdef __x86_64__
  unsigned long long b = 0;
  if (__builtin_ia32_bextr_u64 (b++, 0) != 0)
    abort ();
  if (__builtin_ia32_bextr_u64 (b++, 0x140) != 0)
    abort ();
  if (b != 2)
    abort ();
#endif
}
