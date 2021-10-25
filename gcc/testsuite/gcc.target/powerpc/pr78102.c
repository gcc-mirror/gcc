/* { dg-do compile } */
/* { dg-options "-O2 -mdejagnu-cpu=power8 -DNO_WARN_X86_INTRINSICS" } */
/* { dg-require-effective-target powerpc_p8vector_ok } */

#include <x86intrin.h>

__m128i
foo (const __m128i x, const __m128i y)
{
  return _mm_cmpeq_epi64 (x, y);
}

__v2di
bar (const __v2di x, const __v2di y)
{
  return x == y;
}

__v2di
baz (const __v2di x, const __v2di y)
{
  return x != y;
}
