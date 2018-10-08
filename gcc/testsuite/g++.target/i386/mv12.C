// Test case to check if multiversioning works as expected when the versions
// are defined in different files.

// { dg-do run }
// { dg-require-ifunc "" }
// { dg-options "-O2" }
// { dg-additional-sources "mv12-aux.cc" }

#include "mv12.h"

int main ()
{
  if (__builtin_cpu_supports ("sse4.2"))
    return foo () - 1;
  return foo ();
}

__attribute__ ((target ("default")))
int foo ()
{
  return 0;
}
