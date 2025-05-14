/* Check that calling a neon builtin from a function compiled with vfp fails.  */
/* { dg-do compile } */
/* { dg-require-effective-target arm_neon_h_ok } */
/* { dg-options "-O2" } */
/* { dg-add-options arm_neon_h } */

extern __simd64_int8_t a, b;

__attribute__ ((target ("fpu=vfp")))
void
foo (__simd128_int16_t *p)
{
  *p = (__simd128_int16_t)__builtin_neon_vaddlsv8qi (a, b); /* { dg-error "You must enable NEON instructions .*" } */

}
