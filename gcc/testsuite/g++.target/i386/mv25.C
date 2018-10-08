// Test case to check if Multiversioning works for PCLMUL

// { dg-do run }
// { dg-require-ifunc "" }
// { dg-options "-O2" }

#include <assert.h>

// Check if PCLMUL feature selection works
int foo () __attribute__((target("default")));
int foo () __attribute__((target("pclmul")));

int main ()
{
  int val = foo ();

  if (__builtin_cpu_supports ("pclmul"))
    assert (val == 1);
  else
    assert (val == 0);

  return 0;
}

int __attribute__ ((target("default")))
foo ()
{
  return 0;
}

int __attribute__ ((target("pclmul")))
foo ()
{
  return 1;
}
