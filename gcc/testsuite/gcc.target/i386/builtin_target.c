/* This test checks if the __builtin_cpu_is and __builtin_cpu_supports calls
   are recognized. */

/* { dg-do run } */

#include <assert.h>

int
fn1 ()
{
  /* Check CPU Features.  */
  assert (__builtin_cpu_supports ("cmov") >= 0);

  assert (__builtin_cpu_supports ("mmx") >= 0);

  assert (__builtin_cpu_supports ("popcnt") >= 0);

  assert (__builtin_cpu_supports ("sse") >= 0);

  assert (__builtin_cpu_supports ("sse2") >= 0);

  assert (__builtin_cpu_supports ("sse3") >= 0);

  assert (__builtin_cpu_supports ("ssse3") >= 0);

  assert (__builtin_cpu_supports ("sse4.1") >= 0);

  assert (__builtin_cpu_supports ("sse4.2") >= 0);

  assert (__builtin_cpu_supports ("avx") >= 0);

  assert (__builtin_cpu_supports ("avx2") >= 0);

  /* Check CPU type.  */
  assert (__builtin_cpu_is ("amd") >= 0);

  assert (__builtin_cpu_is ("intel") >= 0);

  assert (__builtin_cpu_is ("atom") >= 0);

  assert (__builtin_cpu_is ("core2") >= 0);

  assert (__builtin_cpu_is ("corei7") >= 0);

  assert (__builtin_cpu_is ("nehalem") >= 0);

  assert (__builtin_cpu_is ("westmere") >= 0);

  assert (__builtin_cpu_is ("sandybridge") >= 0);

  assert (__builtin_cpu_is ("amdfam10h") >= 0);

  assert (__builtin_cpu_is ("barcelona") >= 0);

  assert (__builtin_cpu_is ("shanghai") >= 0);

  assert (__builtin_cpu_is ("istanbul") >= 0);

  assert (__builtin_cpu_is ("amdfam15h") >= 0);

  assert (__builtin_cpu_is ("bdver1") >= 0);

  assert (__builtin_cpu_is ("bdver2") >= 0);

  return 0;
}

int main ()
{
  __builtin_cpu_init ();
  return fn1 ();
}
