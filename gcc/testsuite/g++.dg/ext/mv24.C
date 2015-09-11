// Test case to check if Multiversioning works for AES

// { dg-do run { target i?86-*-* x86_64-*-* } }
// { dg-require-ifunc "" }
// { dg-options "-O2" }

#include <assert.h>

// Check if AES feature selection works
int foo () __attribute__((target("default")));
int foo () __attribute__((target("aes")));

int main ()
{
  int val = foo ();

  if (__builtin_cpu_supports ("aes"))
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

int __attribute__ ((target("aes")))
foo ()
{
  return 1;
}
