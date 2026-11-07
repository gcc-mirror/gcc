// Test that dispatching can choose the right multiversion
// for HYGON CPUs with the same internal GCC processor id

// { dg-do run }
// { dg-require-ifunc "" }
// { dg-options "-O2" }

#include <assert.h>

int __attribute__ ((target("default")))
foo ()
{
  return 0;
}

int __attribute__ ((target("arch=c86-4g-m4"))) foo () {
  return 1;
}

int __attribute__ ((target("arch=c86-4g-m6"))) foo () {
  return 2;
}

int __attribute__ ((target("arch=c86-4g-m7"))) foo () {
  return 3;
}

int __attribute__ ((target("arch=c86-4g-m8"))) foo () {
  return 4;
}

int main ()
{
  int val = foo ();

  if  (__builtin_cpu_is ("c86-4g-m4"))
    assert (val == 1);
  else if (__builtin_cpu_is ("c86-4g-m6"))
    assert (val == 2);
  else if (__builtin_cpu_is ("c86-4g-m7"))
    assert (val == 3);
  else if (__builtin_cpu_is ("c86-4g-m8"))
    assert (val == 4);
  else
    assert (val == 0);

  return 0;
}
