/* { dg-do compile } */
/* { dg-options "-mfp64 -mhard-float -mmsa" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */

typedef double v2f64 __attribute__ ((vector_size (16)));
typedef long long v2i64 __attribute__ ((vector_size (16)));
typedef int v4i32 __attribute__ ((vector_size (16)));

long long foo(const v2f64* a)
{
  v2f64 x;
  v2i64 y;

  x = (v2f64)__builtin_msa_shf_w((v4i32)*a, 0xff);
  y = __builtin_msa_fcun_d(*a, x);

  return y[0];
}

/* { dg-final { scan-assembler-times "ld" 1 } } */
