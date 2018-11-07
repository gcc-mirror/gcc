// Test case to check if Multiversioning works for BMI and BMI2.

// { dg-do run }
// { dg-require-ifunc "" }
// { dg-options "-O2" }

#include <assert.h>

// Check BMI feature selection works
int foo () __attribute__((target("default")));
int foo () __attribute__((target("bmi")));
int foo () __attribute__((target("bmi2")));

// Check specialized versions for archs with BMI is chosen over generic BMI versions.
int bar () __attribute__((target("default")));
int bar () __attribute__((target("bmi")));
int bar () __attribute__((target("bmi2")));
int bar () __attribute__((target("arch=btver2")));
int bar () __attribute__((target("arch=haswell")));

int main ()
{
  int val = foo ();

  if (__builtin_cpu_supports ("bmi2"))
    assert (val == 2);
  else if (__builtin_cpu_supports ("bmi"))
    assert (val == 1);
  else
    assert (val == 0);

  val = bar ();

  if (__builtin_cpu_is ("btver2"))
    assert (val == 5);
  else if (__builtin_cpu_is ("haswell"))
    assert (val == 6);
  else if (__builtin_cpu_supports ("bmi2"))
    assert (val == 2);
  else if (__builtin_cpu_supports ("bmi"))
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

int __attribute__ ((target("bmi")))
foo ()
{
  return 1;
}
int __attribute__ ((target("bmi2")))
foo ()
{
  return 2;
}

int __attribute__ ((target("default")))
bar ()
{
  return 0;
}

int __attribute__ ((target("bmi")))
bar ()
{
  return 1;
}
int __attribute__ ((target("bmi2")))
bar ()
{
  return 2;
}

int __attribute__ ((target("arch=btver2")))
bar ()
{
  return 5;
}

int __attribute__ ((target("arch=haswell")))
bar ()
{
  return 6;
}

