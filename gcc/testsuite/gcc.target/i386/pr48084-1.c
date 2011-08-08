/* { dg-do compile } */
/* { dg-options "-O2 -msse2" } */

typedef int __m64 __attribute__ ((__vector_size__ (8), __may_alias__));
typedef float __v2sf __attribute__ ((__vector_size__ (8)));
typedef float __m128 __attribute__ ((__vector_size__ (16), __may_alias__));
typedef float __v4sf __attribute__ ((__vector_size__ (16)));
void
_mm_storeh_pi (__m64 *__P, __m128 __A)
{
  __builtin_ia32_storehps ((__v2sf *)__P, (__v4sf)__A);
}
