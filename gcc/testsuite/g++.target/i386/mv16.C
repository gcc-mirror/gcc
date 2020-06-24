// Test that dispatching can choose the right multiversion
// for Intel CPUs with the same internal GCC processor id
// but slighly different sets of x86 extensions.

// { dg-do run }
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

int __attribute__ ((target("arch=broadwell"))) foo () {
  return 13;
}

int __attribute__ ((target("arch=skylake"))) foo () {
  return 14;
}

int __attribute__ ((target("arch=skylake-avx512"))) foo () {
  return 15;
}

int __attribute__ ((target("arch=cannonlake"))) foo () {
  return 16;
}

int __attribute__ ((target("arch=icelake-client"))) foo () {
  return 17;
}

int __attribute__ ((target("arch=icelake-server"))) foo () {
  return 18;
}

int __attribute__ ((target("arch=cascadelake"))) foo () {
  return 19;
}

int __attribute__ ((target("arch=tigerlake"))) foo () {
  return 20;
}

int __attribute__ ((target("arch=cooperlake"))) foo () {
  return 21;
}

int __attribute__ ((target("arch=sapphirerapids"))) foo () {
  return 22;
}

int __attribute__ ((target("arch=alderlake"))) foo () {
  return 23;
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
  else if (__builtin_cpu_is ("broadwell"))
    assert (val == 13);
  else if (__builtin_cpu_is ("skylake"))
    assert (val == 14);
  else if (__builtin_cpu_is ("skylake-avx512"))
    assert (val == 15);
  else if (__builtin_cpu_is ("cannonlake"))
    assert (val == 16);
  else if (__builtin_cpu_is ("icelake-client"))
    assert (val == 17);
  else if (__builtin_cpu_is ("icelake-server"))
    assert (val == 18);
  else if (__builtin_cpu_is ("cascadelake"))
    assert (val == 19);
  else if (__builtin_cpu_is ("tigerlake"))
    assert (val == 20);
  else if (__builtin_cpu_is ("cooperlake"))
    assert (val == 21);
  else if (__builtin_cpu_is ("sapphirerapids"))
    assert (val == 22);
  else if (__builtin_cpu_is ("alderlake"))
    assert (val == 23);
  else
    assert (val == 0);

  return 0;
}
