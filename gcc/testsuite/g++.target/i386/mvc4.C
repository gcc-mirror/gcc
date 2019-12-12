/* { dg-do compile } */
/* { dg-require-ifunc "" } */
/* { dg-options "-mavx" } */

#include <immintrin.h>

__m256 x, y, z;

__attribute__((target("avx")))
int bar()
{
  x = _mm256_add_ps (y, z);
  return 1;
}

__attribute__((target("default")))
int bar()
{
  return 2;
}

int
foobar()
{
  if (__builtin_cpu_supports ("avx"))
    return bar();
  else
    return 0;
}

__attribute__((target_clones("default","sse3")))
int foo()
{
  return foobar();
}
