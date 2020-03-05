/* { dg-do compile } */
/* { dg-options "-mabi=64 -mfp64 -mhard-float -mmsa -EL -flax-vector-conversions" } */
/* { dg-skip-if "uses global registers" { *-*-* } { "-O0" } { "" } } */

typedef int v4i32 __attribute__ ((vector_size(16)));
typedef float v4f32 __attribute__ ((vector_size(16)));
typedef double v2f64 __attribute__ ((vector_size(16)));

/* Test that MSA FMADD-like intrinsics do not use first operand for multiplication.  */

register v4i32 a __asm__("$f24");
register v4i32 b __asm__("$f25");
register v4f32 c __asm__("$f26");
register v4f32 d __asm__("$f27");
register v2f64 e __asm__("$f28");
register v2f64 f __asm__("$f29");

void
maddv_b_msa (void)
{
  a = __builtin_msa_maddv_b (a, b, b);
}
/* { dg-final { scan-assembler "maddv\\\.b\t\\\$w24,\\\$w25,\\\$w25" } }  */

void
maddv_h_msa (void)
{
  a = __builtin_msa_maddv_h (a, b, b);
}
/* { dg-final { scan-assembler "maddv\\\.h\t\\\$w24,\\\$w25,\\\$w25" } }  */

void
maddv_w_msa (void)
{
  a = __builtin_msa_maddv_w (a, b, b);
}
/* { dg-final { scan-assembler "maddv\\\.w\t\\\$w24,\\\$w25,\\\$w25" } }  */

void
maddv_d_msa (void)
{
  a = __builtin_msa_maddv_d (a, b, b);
}
/* { dg-final { scan-assembler "maddv\\\.d\t\\\$w24,\\\$w25,\\\$w25" } }  */

void
msubv_b_msa (void)
{
  a = __builtin_msa_msubv_b (a, b, b);
}
/* { dg-final { scan-assembler "msubv\\\.b\t\\\$w24,\\\$w25,\\\$w25" } }  */

void
msubv_h_msa (void)
{
  a = __builtin_msa_msubv_h (a, b, b);
}
/* { dg-final { scan-assembler "msubv\\\.h\t\\\$w24,\\\$w25,\\\$w25" } }  */

void
msubv_w_msa (void)
{
  a = __builtin_msa_msubv_w (a, b, b);
}
/* { dg-final { scan-assembler "msubv\\\.w\t\\\$w24,\\\$w25,\\\$w25" } }  */

void
msubv_d_msa (void)
{
  a = __builtin_msa_msubv_d (a, b, b);
}
/* { dg-final { scan-assembler "msubv\\\.d\t\\\$w24,\\\$w25,\\\$w25" } }  */

void
fmadd_w_msa (void)
{
  c = __builtin_msa_fmadd_w (c, d, d);
}
/* { dg-final { scan-assembler "fmadd\\\.w\t\\\$w26,\\\$w27,\\\$w27" } }  */

void
fmadd_d_msa (void)
{
  e = __builtin_msa_fmadd_d (e, f, f);
}
/* { dg-final { scan-assembler "fmadd\\\.d\t\\\$w28,\\\$w29,\\\$w29" } }  */

void
fmsub_w_msa (void)
{
  c = __builtin_msa_fmsub_w (c, d, d);
}
/* { dg-final { scan-assembler "fmsub\\\.w\t\\\$w26,\\\$w27,\\\$w27" } }  */

void
fmsub_d_msa (void)
{
  e = __builtin_msa_fmsub_d (e, f, f);
}
/* { dg-final { scan-assembler "fmsub\\\.d\t\\\$w28,\\\$w29,\\\$w29" } }  */

