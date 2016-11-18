/* { dg-do compile } */
/* { dg-options "-O3 -ffast-math" } */
/* { dg-prune-output "non-standard ABI extension" } */

typedef float __m128 __attribute__ ((__vector_size__ (16), __may_alias__));

__m128 foo (__m128 a)
{
  return a + a;
}
