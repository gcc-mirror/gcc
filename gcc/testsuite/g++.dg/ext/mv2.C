/* Test case to check if Multiversioning chooses the correct
   dispatching order when versions are for various ISAs.  */
/* { dg-do run { target i?86-*-* x86_64-*-* } } */
/* { dg-require-ifunc "" }  */
/* { dg-options "-O2" } */

#include <assert.h>

/* Default version.  */
int foo () __attribute__ ((target ("default")));
/* The dispatch checks should be in the exact reverse order of the
   declarations below.  */
int foo () __attribute__ ((target ("mmx")));
int foo () __attribute__ ((target ("sse")));
int foo () __attribute__ ((target ("sse2")));
int foo () __attribute__ ((target ("sse3")));
int foo () __attribute__ ((target ("ssse3")));
int foo () __attribute__ ((target ("sse4.1")));
int foo () __attribute__ ((target ("sse4.2")));
int foo () __attribute__ ((target ("popcnt")));
int foo () __attribute__ ((target ("avx")));
int foo () __attribute__ ((target ("avx2")));

int main ()
{
  int val = foo ();

  if (__builtin_cpu_supports ("avx2"))
    assert (val == 1);
  else if (__builtin_cpu_supports ("avx"))
    assert (val == 2);
  else if (__builtin_cpu_supports ("popcnt"))
    assert (val == 3);
  else if (__builtin_cpu_supports ("sse4.2"))
    assert (val == 4);
  else if (__builtin_cpu_supports ("sse4.1"))
    assert (val == 5);
  else if (__builtin_cpu_supports ("ssse3"))
    assert (val == 6);
  else if (__builtin_cpu_supports ("sse3"))
    assert (val == 7);
  else if (__builtin_cpu_supports ("sse2"))
    assert (val == 8);
  else if (__builtin_cpu_supports ("sse"))
    assert (val == 9);
  else if (__builtin_cpu_supports ("mmx"))
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

int __attribute__ ((target("mmx")))
foo ()
{
  return 10;
}

int __attribute__ ((target("sse")))
foo ()
{
  return 9;
}

int __attribute__ ((target("sse2")))
foo ()
{
  return 8;
}

int __attribute__ ((target("sse3")))
foo ()
{
  return 7;
}

int __attribute__ ((target("ssse3")))
foo ()
{
  return 6;
}

int __attribute__ ((target("sse4.1")))
foo ()
{
  return 5;
}

int __attribute__ ((target("sse4.2")))
foo ()
{
  return 4;
}

int __attribute__ ((target("popcnt")))
foo ()
{
  return 3;
}

int __attribute__ ((target("avx")))
foo ()
{
  return 2;
}

int __attribute__ ((target("avx2")))
foo ()
{
  return 1;
}
