// Test that dispatching can choose the right multiversion
// for Intel CPUs with the same internal GCC processor id
// but slighly different sets of x86 extensions.

// { dg-do run { target i?86-*-* x86_64-*-* } }
// { dg-require-ifunc "" }
// { dg-options "-O2" }

#include <assert.h>

int __attribute__ ((target("default")))
foo ()
{
  return 0;
}

int __attribute__ ((target("arch=nehalem")))
foo ()
{
  return 4;
}

int __attribute__ ((target("arch=westmere")))
foo ()
{
  return 5;
}

int __attribute__ ((target("arch=sandybridge")))
foo ()
{
  return 8;
}

int __attribute__ ((target("arch=ivybridge")))
foo ()
{
  return 9;
}

int __attribute__ ((target("arch=haswell")))
foo ()
{
  return 12;
}

int main ()
{
  int val = foo ();

  if (__builtin_cpu_is ("nehalem"))
    assert (val == 4);
  else if (__builtin_cpu_is ("westmere"))
    assert (val == 5);
  else if (__builtin_cpu_is ("sandybridge"))
    assert (val == 8);
  else if (__builtin_cpu_is ("ivybridge"))
    assert (val == 9);
  else if (__builtin_cpu_is ("haswell"))
    assert (val == 12);
  else
    assert (val == 0);

  return 0;
}
