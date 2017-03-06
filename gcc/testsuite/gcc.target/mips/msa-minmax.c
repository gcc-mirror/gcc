/* { dg-do compile } */
/* { dg-options "-mno-mips16 -mfp64 -mhard-float -mmsa" } */

typedef int v4i32 __attribute__ ((vector_size(16)));
typedef float v4f32 __attribute__ ((vector_size(16)));

/* Test MSA signed min/max immediate for correct assembly output.  */

void
min_s_msa (v4i32 *vx, v4i32 *vy)
{
  *vy = __builtin_msa_mini_s_w (*vx, -15);
}
/* { dg-final { scan-assembler "-15" } }  */

void
max_s_msa (v4i32 *vx, v4i32 *vy)
{
  *vy = __builtin_msa_maxi_s_w (*vx, -15);
}
/* { dg-final { scan-assembler "-15" } }  */

/* Test MSA min_a/max_a instructions for forward propagation optimization.  */

#define FUNC(NAME, TYPE, RETTYPE) RETTYPE NAME##_a_msa (TYPE *vx, TYPE *vy) \
{ \
  TYPE dest = __builtin_msa_##NAME##_a_w (*vx, *vy); \
  return dest[0]; \
}

FUNC(fmin, v4f32, float)
/* { dg-final { scan-assembler "fmin_a.w" } }  */
FUNC(fmax, v4f32, float)
/* { dg-final { scan-assembler "fmax_a.w" } }  */
FUNC(min, v4i32, int)
/* { dg-final { scan-assembler "min_a.w" } }  */
FUNC(max, v4i32, int)
/* { dg-final { scan-assembler "max_a.w" } }  */
