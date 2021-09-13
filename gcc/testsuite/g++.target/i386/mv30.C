// PR target/101696
// Test that dispatching can choose the right multiversion
// for x86-64 microarchitecture levels. 

// { dg-do run }
// { dg-require-ifunc "" }
// { dg-options "-O2" }

#include <assert.h>

int __attribute__ ((target("default")))
foo ()
{
  return 0;
}

int __attribute__ ((target("arch=x86-64"))) foo () {
  return 1;
}

int __attribute__ ((target("arch=x86-64-v2"))) foo () {
  return 2;
}

int __attribute__ ((target("arch=x86-64-v3"))) foo () {
  return 3;
}

int __attribute__ ((target("arch=x86-64-v4"))) foo () {
  return 4;
}


int main ()
{
  int val = foo ();

  if (__builtin_cpu_supports ("x86-64-v4"))
    assert (val == 4);
  else if  (__builtin_cpu_supports ("x86-64-v3"))
    assert (val == 3);
  else if  (__builtin_cpu_supports ("x86-64-v2"))
    assert (val == 2);
  else if  (__builtin_cpu_supports ("x86-64"))
    assert (val == 1);
  else
    assert (val == 0);

  return 0;
}
