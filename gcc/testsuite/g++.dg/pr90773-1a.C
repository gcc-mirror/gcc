// { dg-do compile }
// { dg-options "-O2" }
// { dg-additional-options "-mno-avx -msse2 -mtune=skylake" { target { i?86-*-* x86_64-*-* } } }

#include "pr90773-1.h"

int
record_increment(void)
{
  fixed_wide_int_storage x;
  foo (x);
  return 0;
}
