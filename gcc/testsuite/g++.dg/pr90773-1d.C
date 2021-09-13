// { dg-do run }
// { dg-options "-O2" }
// { dg-additional-options "-march=native" { target { i?86-*-* x86_64-*-* } } }
// { dg-additional-sources "pr90773-1a.C" }

#include "pr90773-1.h"

void
foo (fixed_wide_int_storage x)
{
  for (int i = 0; i < x.len; i++)
    if (x.val[i] != i)
      __builtin_abort ();
}

int main ()
{
  return record_increment ();
}
