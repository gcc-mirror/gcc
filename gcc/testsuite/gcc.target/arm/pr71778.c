/* { dg-do compile }  */
/* { dg-require-effective-target arm_neon_ok } */
/* { dg-options "-O2" } */
/* { dg-add-options arm_neon } */

typedef __simd128_int32_t int32x4_t;

__extension__ extern __inline int32x4_t
__attribute__  ((__always_inline__, __gnu_inline__, __artificial__))
vshrq_n_s32 (int32x4_t __a, const int __b)
{
  /* Errors for arm_neon.h intrinsics using constants end up on the line
     in arm_neon.h rather than the source file line.  That means we
     need to put the dg-error up here, rather than on line 22 where we'd
     like it.  */
  return (int32x4_t)__builtin_neon_vshrs_nv4si (__a, __b); /* { dg-error "argument 2 must be a constant immediate" } */
}

int32x4_t
shift (int32x4_t a, int b)
{
  return vshrq_n_s32 (a, b);
}

