// Test that dispatching can choose the right multiversion
// for ZHAOXIN CPU with the same internal GCC processor id

// { dg-do run }
// { dg-require-ifunc "" }
// { dg-options "-O2" }

#include <assert.h>

int __attribute__ ((target("default")))
foo ()
{
  return 0;
}

int __attribute__ ((target("arch=lujiazui"))) foo () {
  return 1;
}


int main ()
{
  int val = foo ();

  if (__builtin_cpu_is ("lujiazui"))
    assert (val == 1);
  else
    assert (val == 0);

  return 0;
}
