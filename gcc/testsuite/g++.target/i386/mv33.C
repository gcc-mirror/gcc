// Test that dispatching can choose the right multiversion
// for avx10.x-512 microarchitecture levels. 

// { dg-do run }
// { dg-require-ifunc "" }
// { dg-options "-O2" }

#include <assert.h>

int __attribute__ ((target("default")))
foo ()
{
  return 0;
}

int __attribute__ ((target("avx10.1-512"))) foo () {
  return 1;
}

int main ()
{
  int val = foo ();

  if  (__builtin_cpu_supports ("avx10.1-512"))
    assert (val == 1);
  else
    assert (val == 0);

  return 0;
}
