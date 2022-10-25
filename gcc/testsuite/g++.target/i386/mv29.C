// Test that dispatching can choose the right multiversion
// for AMD CPUs with the same internal GCC processor id

// { dg-do run }
// { dg-require-ifunc "" }
// { dg-options "-O2" }

#include <assert.h>

int __attribute__ ((target("default")))
foo ()
{
  return 0;
}

int __attribute__ ((target("arch=amdfam10"))) foo () {
  return 1;
}

int __attribute__ ((target("arch=btver1"))) foo () {
  return 2;
}

int __attribute__ ((target("arch=btver2"))) foo () {
  return 3;
}

int __attribute__ ((target("arch=bdver1"))) foo () {
  return 4;
}

int __attribute__ ((target("arch=bdver2"))) foo () {
  return 5;
}

int __attribute__ ((target("arch=bdver3"))) foo () {
  return 6;
}

int __attribute__ ((target("arch=znver1"))) foo () {
  return 7;
}

int __attribute__ ((target("arch=znver2"))) foo () {
  return 8;
}

int __attribute__ ((target("arch=znver3"))) foo () {
  return 9;
}

int __attribute__ ((target("arch=znver4"))) foo () {
  return 10;
}

int main ()
{
  int val = foo ();

  if (__builtin_cpu_is ("amdfam10h"))
    assert (val == 1);
  else if  (__builtin_cpu_is ("btver1"))
    assert (val == 2);
  else if  (__builtin_cpu_is ("btver2"))
    assert (val == 3);
  else if (__builtin_cpu_is ("bdver1"))
    assert (val == 4);
  else if (__builtin_cpu_is ("bdver2"))
    assert (val == 5);
  else if (__builtin_cpu_is ("bdver3"))
    assert (val == 6);
  else if (__builtin_cpu_is ("znver1"))
    assert (val == 7);
  else if (__builtin_cpu_is ("znver2"))
    assert (val == 8);
  else if (__builtin_cpu_is ("znver3"))
    assert (val == 9);
  else if (__builtin_cpu_is ("znver4"))
    assert (val == 10);
  else
    assert (val == 0);

  return 0;
}
