/* { dg-do run { target le } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mdejagnu-cpu=power8 -mvsx -O2 " } */

typedef __attribute__ ((__aligned__ (8))) unsigned long long __m64;
typedef float __m128 __attribute__ ((__vector_size__ (16), __may_alias__));

/* PR84033.  Extracted from xmmintrin.h but with a pointer param to
   allow swaps to happen when not inline.  */
int __attribute__ ((__noinline__))
_mm_movemask_ps (__m128 *__A)
{
  __vector __m64 result;
  static const __vector unsigned int perm_mask =
    {
      0x00204060, 0x80808080, 0x80808080, 0x80808080
    };

  result = (__vector __m64)
    __builtin_vec_vbpermq ((__vector unsigned char) (*__A),
			   (__vector unsigned char) perm_mask);
  return result[1];
}

int
main (void)
{
  union { unsigned int i[4]; __m128 m; } x
    = { 0x80000000, 0x80000000, 0x7fffffff, 0x7fffffff };
  if (_mm_movemask_ps (&x.m) != 3)
    __builtin_abort ();
  return 0;
}
