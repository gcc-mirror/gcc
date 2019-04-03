/* Test case to check if Multiversioning works.  */
/* { dg-do run } */
/* { dg-require-ifunc "" }  */
/* { dg-options "-O2 -fPIC -fcompare-debug=-Wsign-compare" } */

#include <assert.h>

/* Default version.  */
int foo (); // Extra declaration that is merged with the second one.
int foo () __attribute__ ((target("default")));
/* The other versions of foo.  Mix up the ordering and 
   check if the dispatching does it in the order of priority. */
/* Check combination of target attributes.  */
int foo () __attribute__ ((target("arch=corei7,popcnt")));
/* The target operands in this declaration and the definition are re-ordered.
   This should still work.  */
int foo () __attribute__ ((target("ssse3,avx2")));

/* Check for all target attributes for which dispatchers are available.  */
/* Check arch= */
int foo () __attribute__((target("arch=core2")));
int foo () __attribute__((target("arch=corei7")));
int foo () __attribute__((target("arch=atom")));
/* Check ISAs  */
int foo () __attribute__((target("avx")));
int foo () __attribute__ ((target("arch=core2,sse4.2")));
/* Check more arch=.  */
int foo () __attribute__((target("arch=amdfam10")));
int foo () __attribute__((target("arch=bdver1")));
int foo () __attribute__((target("arch=bdver2")));

int (*p)() = &foo;
int main ()
{
  int val = foo ();
  assert (val ==  (*p)());

  /* Check in the exact same order in which the dispatching
     is expected to happen.  */
  if (__builtin_cpu_is ("bdver1"))
    assert (val == 1);
  else if (__builtin_cpu_is ("bdver2"))
    assert (val == 2);
  else if (__builtin_cpu_supports ("avx2")
	   && __builtin_cpu_supports ("ssse3"))
    assert (val == 3);
  else if (__builtin_cpu_supports ("avx"))
    assert (val == 4);
  else if (__builtin_cpu_is ("corei7")
	   && __builtin_cpu_supports ("popcnt"))
    assert (val == 5);
  else if (__builtin_cpu_is ("corei7"))
    assert (val == 6);
  else if (__builtin_cpu_is ("amdfam10h"))
    assert (val == 7);
  else if (__builtin_cpu_is ("core2")
	   && __builtin_cpu_supports ("sse4.2"))
    assert (val == 8);
  else if (__builtin_cpu_is ("core2"))
    assert (val == 9);
  else if (__builtin_cpu_is ("atom"))
    assert (val == 10);
  else
    assert (val == 0);
  
  return 0;
}

int __attribute__ ((target("default")))
foo ()
{
  return 0;
}

int __attribute__ ((target("arch=corei7,popcnt")))
foo ()
{
  return 5;
}
int __attribute__ ((target("avx2,ssse3")))
foo ()
{
  return 3;
}

int __attribute__ ((target("arch=core2")))
foo ()
{
  return 9;
}

int __attribute__ ((target("arch=corei7")))
foo ()
{
  return 6;
}

int __attribute__ ((target("arch=atom")))
foo ()
{
  return 10;
}

int __attribute__ ((target("avx")))
foo ()
{
  return 4;
}

int __attribute__ ((target("arch=core2,sse4.2")))
foo ()
{
  return 8;
}

int __attribute__ ((target("arch=amdfam10")))
foo ()
{
  return 7;
}

int __attribute__ ((target("arch=bdver1")))
foo ()
{
  return 1;
}

int __attribute__ ((target("arch=bdver2")))
foo ()
{
  return 2;
}
