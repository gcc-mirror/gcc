/* Test case to check if Multiversioning works.  */
/* { dg-do run } */
/* { dg-require-ifunc "" }  */
/* { dg-options "-O2 -fPIC -march=x86-64" } */

#include <assert.h>

/* Default version.  */
int foo (); // Extra declaration that is merged with the second one.
int foo () __attribute__ ((target("default")));

int foo () __attribute__ ((target("arch=nehalem")));

int (*p)() = &foo;
int main ()
{
  int val = foo ();
  assert (val ==  (*p)());

  /* Check in the exact same order in which the dispatching
     is expected to happen.  */
  if (__builtin_cpu_is ("corei7"))
    assert (val == 5);
  else
    assert (val == 0);
  
  return 0;
}

int __attribute__ ((target("default")))
foo ()
{
  return 0;
}

int __attribute__ ((target("arch=nehalem")))
foo ()
{
  return 5;
}
