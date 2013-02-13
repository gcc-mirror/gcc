// Test case to check if multiversioning works as expected when the versions
// are defined in different files. Auxiliary file for mv12.C.

// { dg-do compile { target i?86-*-* x86_64-*-* } }
// { dg-require-ifunc "" }
// { dg-options "-O2" }

#include "mv12.h"

__attribute__ ((target ("sse4.2")))
int foo ()
{
  return 1;
}
