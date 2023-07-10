/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-mcmpccxadd -O2 -fno-if-conversion -fno-if-conversion2" } */
/* { dg-final { scan-assembler-not {cmp[lq]?[ \t]+} } } */
/* { dg-final { scan-assembler-times {cmpoxadd[ \t]+} 12 } } */

#include <immintrin.h>

int foo_jg (int *ptr, int v)
{
  if (_cmpccxadd_epi32(ptr, v, 1, _CMPCCX_O) > v)
    return 100;
  return 200;
}

int foo_jl (int *ptr, int v)
{
  if (_cmpccxadd_epi32(ptr, v, 1, _CMPCCX_O) < v)
    return 300;
  return 100;
}

int foo_je(int *ptr, int v)
{
  if (_cmpccxadd_epi32(ptr, v, 1, _CMPCCX_O) == v)
    return 123;
  return 134;
}

int foo_jne(int *ptr, int v)
{
  if (_cmpccxadd_epi32(ptr, v, 1, _CMPCCX_O) != v)
    return 111;
  return 12;
}

int foo_jge(int *ptr, int v)
{
  if (_cmpccxadd_epi32(ptr, v, 1, _CMPCCX_O) >= v)
    return 413;
  return 23;
}

int foo_jle(int *ptr, int v)
{
  if (_cmpccxadd_epi32(ptr, v, 1, _CMPCCX_O) <= v)
    return 3141;
  return 341;
}

int fooq_jg (long long *ptr, long long v)
{
  if (_cmpccxadd_epi64(ptr, v, 1, _CMPCCX_O) > v)
    return 123;
  return 3;
}

int fooq_jl (long long *ptr, long long v)
{
  if (_cmpccxadd_epi64(ptr, v, 1, _CMPCCX_O) < v)
    return 313;
  return 5;
}

int fooq_je(long long *ptr, long long v)
{
  if (_cmpccxadd_epi64(ptr, v, 1, _CMPCCX_O) == v)
    return 1313;
  return 13;
}

int fooq_jne(long long *ptr, long long v)
{
  if (_cmpccxadd_epi64(ptr, v, 1, _CMPCCX_O) != v)
    return 1314;
  return 132;
}

int fooq_jge(long long *ptr, long long v)
{
  if (_cmpccxadd_epi64(ptr, v, 1, _CMPCCX_O) >= v)
    return 14314;
  return 434;
}

int fooq_jle(long long *ptr, long long v)
{
  if (_cmpccxadd_epi64(ptr, v, 1, _CMPCCX_O) <= v)
    return 14414;
  return 43;
}
