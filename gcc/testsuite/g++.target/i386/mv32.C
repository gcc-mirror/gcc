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

int __attribute__ ((target("arch=yongfeng"))) foo () {
  return 2;
}

int __attribute__ ((target("arch=shijidadao"))) foo () {
  return 3;
}

int main ()
{
  int val = foo ();

  if (__builtin_cpu_is ("lujiazui"))
    assert (val == 1);
  else if (__builtin_cpu_is ("yongfeng"))
    assert (val == 2);
  else if (__builtin_cpu_is ("shijidadao"))
    assert (val == 3);
  else
    assert (val == 0);

  return 0;
}
