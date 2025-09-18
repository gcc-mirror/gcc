/* { dg-options "-std=c23" }  */
/* { dg-do compile }  */

/* MOVT (vector to table)
   Variants are also available for:
   [_s8], [_u16], [_s16], [_u32], [_s32], [_u64], [_s64]
   [_bf16], [_f16], [_f32], [_f64]
   void svwrite_lane_zt[_u8] (uint64_t zt0, svuint8_t zt, uint64_t idx)
	__arm_streaming __arm_out ("zt0");  */

#pragma GCC target "+sve2,+sme-lutv2"
static_assert (__ARM_FEATURE_SME_LUTv2 == 1);
#include <arm_sme.h>

void
test_svwrite_lane_zt_explicit_ok (
  svuint8_t zt_u8, svint8_t zt_s8, svuint16_t zt_u16, svint16_t zt_s16,
  svuint32_t zt_u32, svint32_t zt_s32, svuint64_t zt_u64, svint64_t zt_s64,
  svbfloat16_t zt_bf16, svfloat16_t zt_f16, svfloat32_t zt_f32,
  svfloat64_t zt_f64) __arm_streaming __arm_inout ("zt0")
{
  svwrite_lane_zt_u8 (0, zt_u8, 0);
  svwrite_lane_zt_s8 (0, zt_s8, 0);
  svwrite_lane_zt_u16 (0, zt_u16, 0);
  svwrite_lane_zt_s16 (0, zt_s16, 0);
  svwrite_lane_zt_u32 (0, zt_u32, 0);
  svwrite_lane_zt_s32 (0, zt_s32, 0);
  svwrite_lane_zt_u64 (0, zt_u64, 0);
  svwrite_lane_zt_s64 (0, zt_s64, 0);
  svwrite_lane_zt_bf16 (0, zt_bf16, 0);
  svwrite_lane_zt_f16 (0, zt_f16, 0);
  svwrite_lane_zt_f32 (0, zt_f32, 0);
  svwrite_lane_zt_f64 (0, zt_f64, 0);
}

void
test_svwrite_lane_zt_inferred_ok (
  svuint8_t zt_u8, svint8_t zt_s8, svuint16_t zt_u16, svint16_t zt_s16,
  svuint32_t zt_u32, svint32_t zt_s32, svuint64_t zt_u64, svint64_t zt_s64,
  svbfloat16_t zt_bf16, svfloat16_t zt_f16, svfloat32_t zt_f32,
  svfloat64_t zt_f64) __arm_streaming __arm_inout ("zt0")
{
  svwrite_lane_zt (0, zt_u8, 0);
  svwrite_lane_zt (0, zt_s8, 0);
  svwrite_lane_zt (0, zt_u16, 0);
  svwrite_lane_zt (0, zt_s16, 0);
  svwrite_lane_zt (0, zt_u32, 0);
  svwrite_lane_zt (0, zt_s32, 0);
  svwrite_lane_zt (0, zt_u64, 0);
  svwrite_lane_zt (0, zt_s64, 0);
  svwrite_lane_zt (0, zt_bf16, 0);
  svwrite_lane_zt (0, zt_f16, 0);
  svwrite_lane_zt (0, zt_f32, 0);
  svwrite_lane_zt (0, zt_f64, 0);
}

void
test_svwrite_lane_zt_explicit_error_not_streaming (
  svuint8_t zt_u8, svint8_t zt_s8, svuint16_t zt_u16, svint16_t zt_s16,
  svuint32_t zt_u32, svint32_t zt_s32, svuint64_t zt_u64, svint64_t zt_s64,
  svbfloat16_t zt_bf16, svfloat16_t zt_f16, svfloat32_t zt_f32,
  svfloat64_t zt_f64)
{
  svwrite_lane_zt_u8 (0, zt_u8, 0);     /* { dg-error {ACLE function 'svwrite_lane_zt_u8' can only be called when SME streaming mode is enabled} }    */
  svwrite_lane_zt_s8 (0, zt_s8, 0);     /* { dg-error {ACLE function 'svwrite_lane_zt_s8' can only be called when SME streaming mode is enabled} }    */
  svwrite_lane_zt_u16 (0, zt_u16, 0);   /* { dg-error {ACLE function 'svwrite_lane_zt_u16' can only be called when SME streaming mode is enabled} }   */
  svwrite_lane_zt_s16 (0, zt_s16, 0);   /* { dg-error {ACLE function 'svwrite_lane_zt_s16' can only be called when SME streaming mode is enabled} }   */
  svwrite_lane_zt_u32 (0, zt_u32, 0);   /* { dg-error {ACLE function 'svwrite_lane_zt_u32' can only be called when SME streaming mode is enabled} }   */
  svwrite_lane_zt_s32 (0, zt_s32, 0);   /* { dg-error {ACLE function 'svwrite_lane_zt_s32' can only be called when SME streaming mode is enabled} }   */
  svwrite_lane_zt_u64 (0, zt_u64, 0);   /* { dg-error {ACLE function 'svwrite_lane_zt_u64' can only be called when SME streaming mode is enabled} }   */
  svwrite_lane_zt_s64 (0, zt_s64, 0);   /* { dg-error {ACLE function 'svwrite_lane_zt_s64' can only be called when SME streaming mode is enabled} }   */
  svwrite_lane_zt_bf16 (0, zt_bf16, 0); /* { dg-error {ACLE function 'svwrite_lane_zt_bf16' can only be called when SME streaming mode is enabled} }  */
  svwrite_lane_zt_f16 (0, zt_f16, 0);   /* { dg-error {ACLE function 'svwrite_lane_zt_f16' can only be called when SME streaming mode is enabled} }   */
  svwrite_lane_zt_f32 (0, zt_f32, 0);   /* { dg-error {ACLE function 'svwrite_lane_zt_f32' can only be called when SME streaming mode is enabled} }   */
  svwrite_lane_zt_f64 (0, zt_f64, 0);   /* { dg-error {ACLE function 'svwrite_lane_zt_f64' can only be called when SME streaming mode is enabled} }   */
}

void
test_svwrite_lane_zt_inferred_error_not_streaming (
  svuint8_t zt_u8, svint8_t zt_s8, svuint16_t zt_u16, svint16_t zt_s16,
  svuint32_t zt_u32, svint32_t zt_s32, svuint64_t zt_u64, svint64_t zt_s64,
  svbfloat16_t zt_bf16, svfloat16_t zt_f16, svfloat32_t zt_f32,
  svfloat64_t zt_f64)
{
  svwrite_lane_zt (0, zt_u8, 0);   /* { dg-error {ACLE function 'svwrite_lane_zt_u8' can only be called when SME streaming mode is enabled} }    */
  svwrite_lane_zt (0, zt_s8, 0);   /* { dg-error {ACLE function 'svwrite_lane_zt_s8' can only be called when SME streaming mode is enabled} }    */
  svwrite_lane_zt (0, zt_u16, 0);  /* { dg-error {ACLE function 'svwrite_lane_zt_u16' can only be called when SME streaming mode is enabled} }   */
  svwrite_lane_zt (0, zt_s16, 0);  /* { dg-error {ACLE function 'svwrite_lane_zt_s16' can only be called when SME streaming mode is enabled} }   */
  svwrite_lane_zt (0, zt_u32, 0);  /* { dg-error {ACLE function 'svwrite_lane_zt_u32' can only be called when SME streaming mode is enabled} }   */
  svwrite_lane_zt (0, zt_s32, 0);  /* { dg-error {ACLE function 'svwrite_lane_zt_s32' can only be called when SME streaming mode is enabled} }   */
  svwrite_lane_zt (0, zt_u64, 0);  /* { dg-error {ACLE function 'svwrite_lane_zt_u64' can only be called when SME streaming mode is enabled} }   */
  svwrite_lane_zt (0, zt_s64, 0);  /* { dg-error {ACLE function 'svwrite_lane_zt_s64' can only be called when SME streaming mode is enabled} }   */
  svwrite_lane_zt (0, zt_bf16, 0); /* { dg-error {ACLE function 'svwrite_lane_zt_bf16' can only be called when SME streaming mode is enabled} }  */
  svwrite_lane_zt (0, zt_f16, 0);  /* { dg-error {ACLE function 'svwrite_lane_zt_f16' can only be called when SME streaming mode is enabled} }   */
  svwrite_lane_zt (0, zt_f32, 0);  /* { dg-error {ACLE function 'svwrite_lane_zt_f32' can only be called when SME streaming mode is enabled} }   */
  svwrite_lane_zt (0, zt_f64, 0);  /* { dg-error {ACLE function 'svwrite_lane_zt_f64' can only be called when SME streaming mode is enabled} }   */
}

void
test_svwrite_lane_zt_explicit_error_arg_count_mismatch (
  svuint8_t zt_u8, svint8_t zt_s8, svuint16_t zt_u16, svint16_t zt_s16,
  svuint32_t zt_u32, svint32_t zt_s32, svuint64_t zt_u64, svint64_t zt_s64,
  svbfloat16_t zt_bf16, svfloat16_t zt_f16, svfloat32_t zt_f32,
  svfloat64_t zt_f64) __arm_streaming __arm_out ("zt0")
{
  svwrite_lane_zt_u8 ();   /* { dg-error {too few arguments to function 'svwrite_lane_zt_u8'; expected 3, have 0} }    */
  svwrite_lane_zt_s8 ();   /* { dg-error {too few arguments to function 'svwrite_lane_zt_s8'; expected 3, have 0} }    */
  svwrite_lane_zt_u16 ();  /* { dg-error {too few arguments to function 'svwrite_lane_zt_u16'; expected 3, have 0} }   */
  svwrite_lane_zt_s16 ();  /* { dg-error {too few arguments to function 'svwrite_lane_zt_s16'; expected 3, have 0} }   */
  svwrite_lane_zt_u32 ();  /* { dg-error {too few arguments to function 'svwrite_lane_zt_u32'; expected 3, have 0} }   */
  svwrite_lane_zt_s32 ();  /* { dg-error {too few arguments to function 'svwrite_lane_zt_s32'; expected 3, have 0} }   */
  svwrite_lane_zt_u64 ();  /* { dg-error {too few arguments to function 'svwrite_lane_zt_u64'; expected 3, have 0} }   */
  svwrite_lane_zt_s64 ();  /* { dg-error {too few arguments to function 'svwrite_lane_zt_s64'; expected 3, have 0} }   */
  svwrite_lane_zt_bf16 (); /* { dg-error {too few arguments to function 'svwrite_lane_zt_bf16'; expected 3, have 0} }  */
  svwrite_lane_zt_f16 ();  /* { dg-error {too few arguments to function 'svwrite_lane_zt_f16'; expected 3, have 0} }   */
  svwrite_lane_zt_f32 ();  /* { dg-error {too few arguments to function 'svwrite_lane_zt_f32'; expected 3, have 0} }   */
  svwrite_lane_zt_f64 ();  /* { dg-error {too few arguments to function 'svwrite_lane_zt_f64'; expected 3, have 0} }   */

  svwrite_lane_zt_u8 (0);   /* { dg-error {too few arguments to function 'svwrite_lane_zt_u8'; expected 3, have 1} }    */
  svwrite_lane_zt_s8 (0);   /* { dg-error {too few arguments to function 'svwrite_lane_zt_s8'; expected 3, have 1} }    */
  svwrite_lane_zt_u16 (0);  /* { dg-error {too few arguments to function 'svwrite_lane_zt_u16'; expected 3, have 1} }   */
  svwrite_lane_zt_s16 (0);  /* { dg-error {too few arguments to function 'svwrite_lane_zt_s16'; expected 3, have 1} }   */
  svwrite_lane_zt_u32 (0);  /* { dg-error {too few arguments to function 'svwrite_lane_zt_u32'; expected 3, have 1} }   */
  svwrite_lane_zt_s32 (0);  /* { dg-error {too few arguments to function 'svwrite_lane_zt_s32'; expected 3, have 1} }   */
  svwrite_lane_zt_u64 (0);  /* { dg-error {too few arguments to function 'svwrite_lane_zt_u64'; expected 3, have 1} }   */
  svwrite_lane_zt_s64 (0);  /* { dg-error {too few arguments to function 'svwrite_lane_zt_s64'; expected 3, have 1} }   */
  svwrite_lane_zt_bf16 (0); /* { dg-error {too few arguments to function 'svwrite_lane_zt_bf16'; expected 3, have 1} }  */
  svwrite_lane_zt_f16 (0);  /* { dg-error {too few arguments to function 'svwrite_lane_zt_f16'; expected 3, have 1} }   */
  svwrite_lane_zt_f32 (0);  /* { dg-error {too few arguments to function 'svwrite_lane_zt_f32'; expected 3, have 1} }   */
  svwrite_lane_zt_f64 (0);  /* { dg-error {too few arguments to function 'svwrite_lane_zt_f64'; expected 3, have 1} }   */

  svwrite_lane_zt_u8 (0, zt_u8);     /* { dg-error {too few arguments to function 'svwrite_lane_zt_u8'; expected 3, have 2} }    */
  svwrite_lane_zt_s8 (0, zt_s8);     /* { dg-error {too few arguments to function 'svwrite_lane_zt_s8'; expected 3, have 2} }    */
  svwrite_lane_zt_u16 (0, zt_u16);   /* { dg-error {too few arguments to function 'svwrite_lane_zt_u16'; expected 3, have 2} }   */
  svwrite_lane_zt_s16 (0, zt_s16);   /* { dg-error {too few arguments to function 'svwrite_lane_zt_s16'; expected 3, have 2} }   */
  svwrite_lane_zt_u32 (0, zt_u32);   /* { dg-error {too few arguments to function 'svwrite_lane_zt_u32'; expected 3, have 2} }   */
  svwrite_lane_zt_s32 (0, zt_s32);   /* { dg-error {too few arguments to function 'svwrite_lane_zt_s32'; expected 3, have 2} }   */
  svwrite_lane_zt_u64 (0, zt_u64);   /* { dg-error {too few arguments to function 'svwrite_lane_zt_u64'; expected 3, have 2} }   */
  svwrite_lane_zt_s64 (0, zt_s64);   /* { dg-error {too few arguments to function 'svwrite_lane_zt_s64'; expected 3, have 2} }   */
  svwrite_lane_zt_bf16 (0, zt_bf16); /* { dg-error {too few arguments to function 'svwrite_lane_zt_bf16'; expected 3, have 2} }  */
  svwrite_lane_zt_f16 (0, zt_f16);   /* { dg-error {too few arguments to function 'svwrite_lane_zt_f16'; expected 3, have 2} }   */
  svwrite_lane_zt_f32 (0, zt_f32);   /* { dg-error {too few arguments to function 'svwrite_lane_zt_f32'; expected 3, have 2} }   */
  svwrite_lane_zt_f64 (0, zt_f64);   /* { dg-error {too few arguments to function 'svwrite_lane_zt_f64'; expected 3, have 2} }   */

  svwrite_lane_zt_u8 (0, zt_u8, 0, 0);     /* { dg-error {too many arguments to function 'svwrite_lane_zt_u8'; expected 3, have 4} }    */
  svwrite_lane_zt_s8 (0, zt_s8, 0, 0);     /* { dg-error {too many arguments to function 'svwrite_lane_zt_s8'; expected 3, have 4} }    */
  svwrite_lane_zt_u16 (0, zt_u16, 0, 0);   /* { dg-error {too many arguments to function 'svwrite_lane_zt_u16'; expected 3, have 4} }   */
  svwrite_lane_zt_s16 (0, zt_s16, 0, 0);   /* { dg-error {too many arguments to function 'svwrite_lane_zt_s16'; expected 3, have 4} }   */
  svwrite_lane_zt_u32 (0, zt_u32, 0, 0);   /* { dg-error {too many arguments to function 'svwrite_lane_zt_u32'; expected 3, have 4} }   */
  svwrite_lane_zt_s32 (0, zt_s32, 0, 0);   /* { dg-error {too many arguments to function 'svwrite_lane_zt_s32'; expected 3, have 4} }   */
  svwrite_lane_zt_u64 (0, zt_u64, 0, 0);   /* { dg-error {too many arguments to function 'svwrite_lane_zt_u64'; expected 3, have 4} }   */
  svwrite_lane_zt_s64 (0, zt_s64, 0, 0);   /* { dg-error {too many arguments to function 'svwrite_lane_zt_s64'; expected 3, have 4} }   */
  svwrite_lane_zt_bf16 (0, zt_bf16, 0, 0); /* { dg-error {too many arguments to function 'svwrite_lane_zt_bf16'; expected 3, have 4} }  */
  svwrite_lane_zt_f16 (0, zt_f16, 0, 0);   /* { dg-error {too many arguments to function 'svwrite_lane_zt_f16'; expected 3, have 4} }   */
  svwrite_lane_zt_f32 (0, zt_f32, 0, 0);   /* { dg-error {too many arguments to function 'svwrite_lane_zt_f32'; expected 3, have 4} }   */
  svwrite_lane_zt_f64 (0, zt_f64, 0, 0);   /* { dg-error {too many arguments to function 'svwrite_lane_zt_f64'; expected 3, have 4} }   */
}

void
test_svwrite_lane_zt_implicit_error_arg_count_mismatch (
  svuint8_t zt_u8, svint8_t zt_s8, svuint16_t zt_u16, svint16_t zt_s16,
  svuint32_t zt_u32, svint32_t zt_s32, svuint64_t zt_u64, svint64_t zt_s64,
  svbfloat16_t zt_bf16, svfloat16_t zt_f16, svfloat32_t zt_f32,
  svfloat64_t zt_f64) __arm_streaming __arm_out ("zt0")
{
  svwrite_lane_zt ();           /* { dg-error {too few arguments to function 'svwrite_lane_zt'} }  */
  svwrite_lane_zt (0);          /* { dg-error {too few arguments to function 'svwrite_lane_zt'} }  */
  svwrite_lane_zt (0, zt_u8);   /* { dg-error {too few arguments to function 'svwrite_lane_zt'} }  */
  svwrite_lane_zt (0, zt_s8);   /* { dg-error {too few arguments to function 'svwrite_lane_zt'} }  */
  svwrite_lane_zt (0, zt_u16);	/* { dg-error {too few arguments to function 'svwrite_lane_zt'} }  */
  svwrite_lane_zt (0, zt_s16);	/* { dg-error {too few arguments to function 'svwrite_lane_zt'} }  */
  svwrite_lane_zt (0, zt_u32);	/* { dg-error {too few arguments to function 'svwrite_lane_zt'} }  */
  svwrite_lane_zt (0, zt_s32);	/* { dg-error {too few arguments to function 'svwrite_lane_zt'} }  */
  svwrite_lane_zt (0, zt_u64);	/* { dg-error {too few arguments to function 'svwrite_lane_zt'} }  */
  svwrite_lane_zt (0, zt_s64);	/* { dg-error {too few arguments to function 'svwrite_lane_zt'} }  */
  svwrite_lane_zt (0, zt_bf16); /* { dg-error {too few arguments to function 'svwrite_lane_zt'} }  */
  svwrite_lane_zt (0, zt_f16);	/* { dg-error {too few arguments to function 'svwrite_lane_zt'} }  */
  svwrite_lane_zt (0, zt_f32);	/* { dg-error {too few arguments to function 'svwrite_lane_zt'} }  */
  svwrite_lane_zt (0, zt_f64);	/* { dg-error {too few arguments to function 'svwrite_lane_zt'} }  */

  svwrite_lane_zt (0, zt_u8, 0, 0);   /* { dg-error {too many arguments to function 'svwrite_lane_zt'} }  */
  svwrite_lane_zt (0, zt_s8, 0, 0);   /* { dg-error {too many arguments to function 'svwrite_lane_zt'} }  */
  svwrite_lane_zt (0, zt_u16, 0, 0);  /* { dg-error {too many arguments to function 'svwrite_lane_zt'} }  */
  svwrite_lane_zt (0, zt_s16, 0, 0);  /* { dg-error {too many arguments to function 'svwrite_lane_zt'} }  */
  svwrite_lane_zt (0, zt_u32, 0, 0);  /* { dg-error {too many arguments to function 'svwrite_lane_zt'} }  */
  svwrite_lane_zt (0, zt_s32, 0, 0);  /* { dg-error {too many arguments to function 'svwrite_lane_zt'} }  */
  svwrite_lane_zt (0, zt_u64, 0, 0);  /* { dg-error {too many arguments to function 'svwrite_lane_zt'} }  */
  svwrite_lane_zt (0, zt_s64, 0, 0);  /* { dg-error {too many arguments to function 'svwrite_lane_zt'} }  */
  svwrite_lane_zt (0, zt_bf16, 0, 0); /* { dg-error {too many arguments to function 'svwrite_lane_zt'} }  */
  svwrite_lane_zt (0, zt_f16, 0, 0);  /* { dg-error {too many arguments to function 'svwrite_lane_zt'} }  */
  svwrite_lane_zt (0, zt_f32, 0, 0);  /* { dg-error {too many arguments to function 'svwrite_lane_zt'} }  */
  svwrite_lane_zt (0, zt_f64, 0, 0);  /* { dg-error {too many arguments to function 'svwrite_lane_zt'} }  */
}

void
test_svwrite_lane_zt_explicit_error_arg_type_mismatch (
  svuint8_t zt_u8, svint8_t zt_s8, svuint16_t zt_u16, svint16_t zt_s16,
  svuint32_t zt_u32, svint32_t zt_s32, svuint64_t zt_u64, svint64_t zt_s64,
  svbfloat16_t zt_bf16, svfloat16_t zt_f16, svfloat32_t zt_f32,
  svfloat64_t zt_f64, svuint8x2_t zt_u8x2, svint8x2_t zt_s8x2,
  svuint16x2_t zt_u16x2, svint16x2_t zt_s16x2, svuint32x2_t zt_u32x2,
  svint32x2_t zt_s32x2, svuint64x2_t zt_u64x2, svint64x2_t zt_s64x2,
  svbfloat16x2_t zt_bf16x2, svfloat16x2_t zt_f16x2, svfloat32x2_t zt_f32x2,
  svfloat64x2_t zt_f64x2) __arm_streaming __arm_out ("zt0")
{
  struct Foo { uint64_t val; } foo = {0};
  svwrite_lane_zt_u8 (foo, zt_u8, 0);     /* { dg-error {incompatible type for argument 1 of 'svwrite_lane_zt_u8'} }    */
  svwrite_lane_zt_s8 (foo, zt_s8, 0);     /* { dg-error {incompatible type for argument 1 of 'svwrite_lane_zt_s8'} }    */
  svwrite_lane_zt_u16 (foo, zt_u16, 0);   /* { dg-error {incompatible type for argument 1 of 'svwrite_lane_zt_u16'} }   */
  svwrite_lane_zt_s16 (foo, zt_s16, 0);   /* { dg-error {incompatible type for argument 1 of 'svwrite_lane_zt_s16'} }   */
  svwrite_lane_zt_u32 (foo, zt_u32, 0);   /* { dg-error {incompatible type for argument 1 of 'svwrite_lane_zt_u32'} }   */
  svwrite_lane_zt_s32 (foo, zt_s32, 0);   /* { dg-error {incompatible type for argument 1 of 'svwrite_lane_zt_s32'} }   */
  svwrite_lane_zt_u64 (foo, zt_u64, 0);   /* { dg-error {incompatible type for argument 1 of 'svwrite_lane_zt_u64'} }   */
  svwrite_lane_zt_s64 (foo, zt_s64, 0);   /* { dg-error {incompatible type for argument 1 of 'svwrite_lane_zt_s64'} }   */
  svwrite_lane_zt_bf16 (foo, zt_bf16, 0); /* { dg-error {incompatible type for argument 1 of 'svwrite_lane_zt_bf16'} }  */
  svwrite_lane_zt_f16 (foo, zt_f16, 0);   /* { dg-error {incompatible type for argument 1 of 'svwrite_lane_zt_f16'} }   */
  svwrite_lane_zt_f32 (foo, zt_f32, 0);   /* { dg-error {incompatible type for argument 1 of 'svwrite_lane_zt_f32'} }   */
  svwrite_lane_zt_f64 (foo, zt_f64, 0);   /* { dg-error {incompatible type for argument 1 of 'svwrite_lane_zt_f64'} }   */

  svwrite_lane_zt_u8 (0, zt_s8, 0);    /* { dg-error {incompatible type for argument 2 of 'svwrite_lane_zt_u8'} }    */
  svwrite_lane_zt_s8 (0, zt_u8, 0);    /* { dg-error {incompatible type for argument 2 of 'svwrite_lane_zt_s8'} }    */
  svwrite_lane_zt_u16 (0, zt_s16, 0);  /* { dg-error {incompatible type for argument 2 of 'svwrite_lane_zt_u16'} }   */
  svwrite_lane_zt_s16 (0, zt_u16, 0);  /* { dg-error {incompatible type for argument 2 of 'svwrite_lane_zt_s16'} }   */
  svwrite_lane_zt_u32 (0, zt_s32, 0);  /* { dg-error {incompatible type for argument 2 of 'svwrite_lane_zt_u32'} }   */
  svwrite_lane_zt_s32 (0, zt_u32, 0);  /* { dg-error {incompatible type for argument 2 of 'svwrite_lane_zt_s32'} }   */
  svwrite_lane_zt_u64 (0, zt_s64, 0);  /* { dg-error {incompatible type for argument 2 of 'svwrite_lane_zt_u64'} }   */
  svwrite_lane_zt_s64 (0, zt_u64, 0);  /* { dg-error {incompatible type for argument 2 of 'svwrite_lane_zt_s64'} }   */
  svwrite_lane_zt_bf16 (0, zt_f16, 0); /* { dg-error {incompatible type for argument 2 of 'svwrite_lane_zt_bf16'} }  */
  svwrite_lane_zt_f16 (0, zt_bf16, 0); /* { dg-error {incompatible type for argument 2 of 'svwrite_lane_zt_f16'} }   */
  svwrite_lane_zt_f32 (0, zt_f64, 0);  /* { dg-error {incompatible type for argument 2 of 'svwrite_lane_zt_f32'} }   */
  svwrite_lane_zt_f64 (0, zt_f32, 0);  /* { dg-error {incompatible type for argument 2 of 'svwrite_lane_zt_f64'} }   */

  svwrite_lane_zt_u8 (0, zt_u8x2, 0);     /* { dg-error {incompatible type for argument 2 of 'svwrite_lane_zt_u8'} }    */
  svwrite_lane_zt_s8 (0, zt_s8x2, 0);     /* { dg-error {incompatible type for argument 2 of 'svwrite_lane_zt_s8'} }    */
  svwrite_lane_zt_u16 (0, zt_u16x2, 0);   /* { dg-error {incompatible type for argument 2 of 'svwrite_lane_zt_u16'} }   */
  svwrite_lane_zt_s16 (0, zt_s16x2, 0);   /* { dg-error {incompatible type for argument 2 of 'svwrite_lane_zt_s16'} }   */
  svwrite_lane_zt_u32 (0, zt_u32x2, 0);   /* { dg-error {incompatible type for argument 2 of 'svwrite_lane_zt_u32'} }   */
  svwrite_lane_zt_s32 (0, zt_s32x2, 0);   /* { dg-error {incompatible type for argument 2 of 'svwrite_lane_zt_s32'} }   */
  svwrite_lane_zt_u64 (0, zt_u64x2, 0);   /* { dg-error {incompatible type for argument 2 of 'svwrite_lane_zt_u64'} }   */
  svwrite_lane_zt_s64 (0, zt_s64x2, 0);   /* { dg-error {incompatible type for argument 2 of 'svwrite_lane_zt_s64'} }   */
  svwrite_lane_zt_bf16 (0, zt_bf16x2, 0); /* { dg-error {incompatible type for argument 2 of 'svwrite_lane_zt_bf16'} }  */
  svwrite_lane_zt_f16 (0, zt_f16x2, 0);   /* { dg-error {incompatible type for argument 2 of 'svwrite_lane_zt_f16'} }   */
  svwrite_lane_zt_f32 (0, zt_f32x2, 0);   /* { dg-error {incompatible type for argument 2 of 'svwrite_lane_zt_f32'} }   */
  svwrite_lane_zt_f64 (0, zt_f64x2, 0);   /* { dg-error {incompatible type for argument 2 of 'svwrite_lane_zt_f64'} }   */

  svwrite_lane_zt_u8 (0, zt_u8, foo);     /* { dg-error {incompatible type for argument 3 of 'svwrite_lane_zt_u8'} }    */
  svwrite_lane_zt_s8 (0, zt_s8, foo);     /* { dg-error {incompatible type for argument 3 of 'svwrite_lane_zt_s8'} }    */
  svwrite_lane_zt_u16 (0, zt_u16, foo);   /* { dg-error {incompatible type for argument 3 of 'svwrite_lane_zt_u16'} }   */
  svwrite_lane_zt_s16 (0, zt_s16, foo);   /* { dg-error {incompatible type for argument 3 of 'svwrite_lane_zt_s16'} }   */
  svwrite_lane_zt_u32 (0, zt_u32, foo);   /* { dg-error {incompatible type for argument 3 of 'svwrite_lane_zt_u32'} }   */
  svwrite_lane_zt_s32 (0, zt_s32, foo);   /* { dg-error {incompatible type for argument 3 of 'svwrite_lane_zt_s32'} }   */
  svwrite_lane_zt_u64 (0, zt_u64, foo);   /* { dg-error {incompatible type for argument 3 of 'svwrite_lane_zt_u64'} }   */
  svwrite_lane_zt_s64 (0, zt_s64, foo);   /* { dg-error {incompatible type for argument 3 of 'svwrite_lane_zt_s64'} }   */
  svwrite_lane_zt_bf16 (0, zt_bf16, foo); /* { dg-error {incompatible type for argument 3 of 'svwrite_lane_zt_bf16'} }  */
  svwrite_lane_zt_f16 (0, zt_f16, foo);   /* { dg-error {incompatible type for argument 3 of 'svwrite_lane_zt_f16'} }   */
  svwrite_lane_zt_f32 (0, zt_f32, foo);   /* { dg-error {incompatible type for argument 3 of 'svwrite_lane_zt_f32'} }   */
  svwrite_lane_zt_f64 (0, zt_f64, foo);   /* { dg-error {incompatible type for argument 3 of 'svwrite_lane_zt_f64'} }   */
}

void
test_svwrite_zt_inferred_error_arg_type_mismatch (
  svuint8_t zt_u8, svint8_t zt_s8, svuint16_t zt_u16, svint16_t zt_s16,
  svuint32_t zt_u32, svint32_t zt_s32, svuint64_t zt_u64, svint64_t zt_s64,
  svbfloat16_t zt_bf16, svfloat16_t zt_f16, svfloat32_t zt_f32,
  svfloat64_t zt_f64, svuint8x2_t zt_u8x2) __arm_streaming __arm_out ("zt0")
{
  struct Foo { uint64_t val; } foo = {0};
  svwrite_lane_zt (foo, zt_u8, 0);  /* { dg-error {passing 'struct Foo' to argument 1 of 'svwrite_lane_zt', which expects 'uint64_t'} }  */
  svwrite_lane_zt (0, zt_u8x2, 0);  /* { dg-error {passing 'svuint8x2_t' to argument 2 of 'svwrite_lane_zt', which expects a single SVE vector rather than a tuple} }  */
  svwrite_lane_zt (0, zt_u8, foo);  /* { dg-error {passing 'struct Foo' to argument 3 of 'svwrite_lane_zt', which expects 'uint64_t'} }  */
}

void
test_svwrite_lane_zt_explicit_error_zt0_not_immediate (
  uint64_t zt0, svuint8_t zt_u8, svint8_t zt_s8, svuint16_t zt_u16,
  svint16_t zt_s16, svuint32_t zt_u32, svint32_t zt_s32, svuint64_t zt_u64,
  svint64_t zt_s64, svbfloat16_t zt_bf16, svfloat16_t zt_f16,
  svfloat32_t zt_f32, svfloat64_t zt_f64) __arm_streaming __arm_out ("zt0")
{
  svwrite_lane_zt_u8 (zt0, zt_u8, 0);     /* { dg-error {argument 1 of 'svwrite_lane_zt_u8' must be an integer constant expression} }    */
  svwrite_lane_zt_s8 (zt0, zt_s8, 0);     /* { dg-error {argument 1 of 'svwrite_lane_zt_s8' must be an integer constant expression} }    */
  svwrite_lane_zt_u16 (zt0, zt_u16, 0);   /* { dg-error {argument 1 of 'svwrite_lane_zt_u16' must be an integer constant expression} }   */
  svwrite_lane_zt_s16 (zt0, zt_s16, 0);   /* { dg-error {argument 1 of 'svwrite_lane_zt_s16' must be an integer constant expression} }   */
  svwrite_lane_zt_u32 (zt0, zt_u32, 0);   /* { dg-error {argument 1 of 'svwrite_lane_zt_u32' must be an integer constant expression} }   */
  svwrite_lane_zt_s32 (zt0, zt_s32, 0);   /* { dg-error {argument 1 of 'svwrite_lane_zt_s32' must be an integer constant expression} }   */
  svwrite_lane_zt_u64 (zt0, zt_u64, 0);   /* { dg-error {argument 1 of 'svwrite_lane_zt_u64' must be an integer constant expression} }   */
  svwrite_lane_zt_s64 (zt0, zt_s64, 0);   /* { dg-error {argument 1 of 'svwrite_lane_zt_s64' must be an integer constant expression} }   */
  svwrite_lane_zt_bf16 (zt0, zt_bf16, 0); /* { dg-error {argument 1 of 'svwrite_lane_zt_bf16' must be an integer constant expression} }  */
  svwrite_lane_zt_f16 (zt0, zt_f16, 0);   /* { dg-error {argument 1 of 'svwrite_lane_zt_f16' must be an integer constant expression} }   */
  svwrite_lane_zt_f32 (zt0, zt_f32, 0);   /* { dg-error {argument 1 of 'svwrite_lane_zt_f32' must be an integer constant expression} }   */
  svwrite_lane_zt_f64 (zt0, zt_f64, 0);   /* { dg-error {argument 1 of 'svwrite_lane_zt_f64' must be an integer constant expression} }   */
}

void
test_svwrite_lane_zt_inferred_error_zt0_not_immediate (
  uint64_t zt0, svuint8_t zt_u8, svint8_t zt_s8, svuint16_t zt_u16,
  svint16_t zt_s16, svuint32_t zt_u32, svint32_t zt_s32, svuint64_t zt_u64,
  svint64_t zt_s64, svbfloat16_t zt_bf16, svfloat16_t zt_f16,
  svfloat32_t zt_f32, svfloat64_t zt_f64) __arm_streaming __arm_out ("zt0")
{
  svwrite_lane_zt (zt0, zt_u8, 0);   /* { dg-error {argument 1 of 'svwrite_lane_zt' must be an integer constant expression} }  */
  svwrite_lane_zt (zt0, zt_s8, 0);   /* { dg-error {argument 1 of 'svwrite_lane_zt' must be an integer constant expression} }  */
  svwrite_lane_zt (zt0, zt_u16, 0);  /* { dg-error {argument 1 of 'svwrite_lane_zt' must be an integer constant expression} }  */
  svwrite_lane_zt (zt0, zt_s16, 0);  /* { dg-error {argument 1 of 'svwrite_lane_zt' must be an integer constant expression} }  */
  svwrite_lane_zt (zt0, zt_u32, 0);  /* { dg-error {argument 1 of 'svwrite_lane_zt' must be an integer constant expression} }  */
  svwrite_lane_zt (zt0, zt_s32, 0);  /* { dg-error {argument 1 of 'svwrite_lane_zt' must be an integer constant expression} }  */
  svwrite_lane_zt (zt0, zt_u64, 0);  /* { dg-error {argument 1 of 'svwrite_lane_zt' must be an integer constant expression} }  */
  svwrite_lane_zt (zt0, zt_s64, 0);  /* { dg-error {argument 1 of 'svwrite_lane_zt' must be an integer constant expression} }  */
  svwrite_lane_zt (zt0, zt_bf16, 0); /* { dg-error {argument 1 of 'svwrite_lane_zt' must be an integer constant expression} }  */
  svwrite_lane_zt (zt0, zt_f16, 0);  /* { dg-error {argument 1 of 'svwrite_lane_zt' must be an integer constant expression} }  */
  svwrite_lane_zt (zt0, zt_f32, 0);  /* { dg-error {argument 1 of 'svwrite_lane_zt' must be an integer constant expression} }  */
  svwrite_lane_zt (zt0, zt_f64, 0);  /* { dg-error {argument 1 of 'svwrite_lane_zt' must be an integer constant expression} }  */
}

void
test_svwrite_lane_zt_explicit_error_idx_not_immediate (
  uint64_t idx, svuint8_t zt_u8, svint8_t zt_s8, svuint16_t zt_u16,
  svint16_t zt_s16, svuint32_t zt_u32, svint32_t zt_s32, svuint64_t zt_u64,
  svint64_t zt_s64, svbfloat16_t zt_bf16, svfloat16_t zt_f16,
  svfloat32_t zt_f32, svfloat64_t zt_f64) __arm_streaming __arm_out ("zt0")
{
  svwrite_lane_zt_u8 (0, zt_u8, idx);      /* { dg-error {argument 3 of 'svwrite_lane_zt_u8' must be an integer constant expression} }    */
  svwrite_lane_zt_s8 (0, zt_s8, idx);      /* { dg-error {argument 3 of 'svwrite_lane_zt_s8' must be an integer constant expression} }    */
  svwrite_lane_zt_u16 (0, zt_u16, idx);    /* { dg-error {argument 3 of 'svwrite_lane_zt_u16' must be an integer constant expression} }   */
  svwrite_lane_zt_s16 (0, zt_s16, idx);    /* { dg-error {argument 3 of 'svwrite_lane_zt_s16' must be an integer constant expression} }   */
  svwrite_lane_zt_u32 (0, zt_u32, idx);    /* { dg-error {argument 3 of 'svwrite_lane_zt_u32' must be an integer constant expression} }   */
  svwrite_lane_zt_s32 (0, zt_s32, idx);    /* { dg-error {argument 3 of 'svwrite_lane_zt_s32' must be an integer constant expression} }   */
  svwrite_lane_zt_u64 (0, zt_u64, idx);    /* { dg-error {argument 3 of 'svwrite_lane_zt_u64' must be an integer constant expression} }   */
  svwrite_lane_zt_s64 (0, zt_s64, idx);    /* { dg-error {argument 3 of 'svwrite_lane_zt_s64' must be an integer constant expression} }   */
  svwrite_lane_zt_bf16 (0, zt_bf16, idx);  /* { dg-error {argument 3 of 'svwrite_lane_zt_bf16' must be an integer constant expression} }  */
  svwrite_lane_zt_f16 (0, zt_f16, idx);    /* { dg-error {argument 3 of 'svwrite_lane_zt_f16' must be an integer constant expression} }   */
  svwrite_lane_zt_f32 (0, zt_f32, idx);    /* { dg-error {argument 3 of 'svwrite_lane_zt_f32' must be an integer constant expression} }   */
  svwrite_lane_zt_f64 (0, zt_f64, idx);    /* { dg-error {argument 3 of 'svwrite_lane_zt_f64' must be an integer constant expression} }   */
}

void
test_svwrite_lane_zt_inferred_error_idx_not_immediate (
  uint64_t idx, svuint8_t zt_u8, svint8_t zt_s8, svuint16_t zt_u16,
  svint16_t zt_s16, svuint32_t zt_u32, svint32_t zt_s32, svuint64_t zt_u64,
  svint64_t zt_s64, svbfloat16_t zt_bf16, svfloat16_t zt_f16,
  svfloat32_t zt_f32, svfloat64_t zt_f64) __arm_streaming __arm_out ("zt0")
{
  svwrite_lane_zt (0, zt_u8, idx);   /* { dg-error {argument 3 of 'svwrite_lane_zt' must be an integer constant expression} }  */
  svwrite_lane_zt (0, zt_s8, idx);   /* { dg-error {argument 3 of 'svwrite_lane_zt' must be an integer constant expression} }  */
  svwrite_lane_zt (0, zt_u16, idx);  /* { dg-error {argument 3 of 'svwrite_lane_zt' must be an integer constant expression} }  */
  svwrite_lane_zt (0, zt_s16, idx);  /* { dg-error {argument 3 of 'svwrite_lane_zt' must be an integer constant expression} }  */
  svwrite_lane_zt (0, zt_u32, idx);  /* { dg-error {argument 3 of 'svwrite_lane_zt' must be an integer constant expression} }  */
  svwrite_lane_zt (0, zt_s32, idx);  /* { dg-error {argument 3 of 'svwrite_lane_zt' must be an integer constant expression} }  */
  svwrite_lane_zt (0, zt_u64, idx);  /* { dg-error {argument 3 of 'svwrite_lane_zt' must be an integer constant expression} }  */
  svwrite_lane_zt (0, zt_s64, idx);  /* { dg-error {argument 3 of 'svwrite_lane_zt' must be an integer constant expression} }  */
  svwrite_lane_zt (0, zt_bf16, idx); /* { dg-error {argument 3 of 'svwrite_lane_zt' must be an integer constant expression} }  */
  svwrite_lane_zt (0, zt_f16, idx);  /* { dg-error {argument 3 of 'svwrite_lane_zt' must be an integer constant expression} }  */
  svwrite_lane_zt (0, zt_f32, idx);  /* { dg-error {argument 3 of 'svwrite_lane_zt' must be an integer constant expression} }  */
  svwrite_lane_zt (0, zt_f64, idx);  /* { dg-error {argument 3 of 'svwrite_lane_zt' must be an integer constant expression} }  */
}

void
test_svwrite_lane_zt_explicit_error_idx_not_in_range (
  uint64_t idx, svuint8_t zt_u8, svint8_t zt_s8, svuint16_t zt_u16,
  svint16_t zt_s16, svuint32_t zt_u32, svint32_t zt_s32, svuint64_t zt_u64,
  svint64_t zt_s64, svbfloat16_t zt_bf16, svfloat16_t zt_f16,
  svfloat32_t zt_f32, svfloat64_t zt_f64) __arm_streaming __arm_out ("zt0")
{
  svwrite_lane_zt_u8 (0, zt_u8, 4);     /* { dg-error {passing 4 to argument 3 of 'svwrite_lane_zt_u8', which expects a value in the range \[0, 3\]} }    */
  svwrite_lane_zt_s8 (0, zt_s8, 4);     /* { dg-error {passing 4 to argument 3 of 'svwrite_lane_zt_s8', which expects a value in the range \[0, 3\]} }    */
  svwrite_lane_zt_u16 (0, zt_u16, 4);   /* { dg-error {passing 4 to argument 3 of 'svwrite_lane_zt_u16', which expects a value in the range \[0, 3\]} }   */
  svwrite_lane_zt_s16 (0, zt_s16, 4);   /* { dg-error {passing 4 to argument 3 of 'svwrite_lane_zt_s16', which expects a value in the range \[0, 3\]} }   */
  svwrite_lane_zt_u32 (0, zt_u32, 4);   /* { dg-error {passing 4 to argument 3 of 'svwrite_lane_zt_u32', which expects a value in the range \[0, 3\]} }   */
  svwrite_lane_zt_s32 (0, zt_s32, 4);   /* { dg-error {passing 4 to argument 3 of 'svwrite_lane_zt_s32', which expects a value in the range \[0, 3\]} }   */
  svwrite_lane_zt_u64 (0, zt_u64, 4);   /* { dg-error {passing 4 to argument 3 of 'svwrite_lane_zt_u64', which expects a value in the range \[0, 3\]} }   */
  svwrite_lane_zt_s64 (0, zt_s64, 4);   /* { dg-error {passing 4 to argument 3 of 'svwrite_lane_zt_s64', which expects a value in the range \[0, 3\]} }   */
  svwrite_lane_zt_bf16 (0, zt_bf16, 4); /* { dg-error {passing 4 to argument 3 of 'svwrite_lane_zt_bf16', which expects a value in the range \[0, 3\]} }  */
  svwrite_lane_zt_f16 (0, zt_f16, 4);   /* { dg-error {passing 4 to argument 3 of 'svwrite_lane_zt_f16', which expects a value in the range \[0, 3\]} }   */
  svwrite_lane_zt_f32 (0, zt_f32, 4);   /* { dg-error {passing 4 to argument 3 of 'svwrite_lane_zt_f32', which expects a value in the range \[0, 3\]} }   */
  svwrite_lane_zt_f64 (0, zt_f64, 4);   /* { dg-error {passing 4 to argument 3 of 'svwrite_lane_zt_f64', which expects a value in the range \[0, 3\]} }   */
}

void
test_svwrite_lane_zt_inferred_error_idx_not_in_range (
  uint64_t idx, svuint8_t zt_u8, svint8_t zt_s8, svuint16_t zt_u16,
  svint16_t zt_s16, svuint32_t zt_u32, svint32_t zt_s32, svuint64_t zt_u64,
  svint64_t zt_s64, svbfloat16_t zt_bf16, svfloat16_t zt_f16,
  svfloat32_t zt_f32, svfloat64_t zt_f64) __arm_streaming __arm_out ("zt0")
{
  svwrite_lane_zt (0, zt_u8, 4);   /* { dg-error {passing 4 to argument 3 of 'svwrite_lane_zt', which expects a value in the range \[0, 3\]} }  */
  svwrite_lane_zt (0, zt_s8, 4);   /* { dg-error {passing 4 to argument 3 of 'svwrite_lane_zt', which expects a value in the range \[0, 3\]} }  */
  svwrite_lane_zt (0, zt_u16, 4);  /* { dg-error {passing 4 to argument 3 of 'svwrite_lane_zt', which expects a value in the range \[0, 3\]} }  */
  svwrite_lane_zt (0, zt_s16, 4);  /* { dg-error {passing 4 to argument 3 of 'svwrite_lane_zt', which expects a value in the range \[0, 3\]} }  */
  svwrite_lane_zt (0, zt_u32, 4);  /* { dg-error {passing 4 to argument 3 of 'svwrite_lane_zt', which expects a value in the range \[0, 3\]} }  */
  svwrite_lane_zt (0, zt_s32, 4);  /* { dg-error {passing 4 to argument 3 of 'svwrite_lane_zt', which expects a value in the range \[0, 3\]} }  */
  svwrite_lane_zt (0, zt_u64, 4);  /* { dg-error {passing 4 to argument 3 of 'svwrite_lane_zt', which expects a value in the range \[0, 3\]} }  */
  svwrite_lane_zt (0, zt_s64, 4);  /* { dg-error {passing 4 to argument 3 of 'svwrite_lane_zt', which expects a value in the range \[0, 3\]} }  */
  svwrite_lane_zt (0, zt_bf16, 4); /* { dg-error {passing 4 to argument 3 of 'svwrite_lane_zt', which expects a value in the range \[0, 3\]} }  */
  svwrite_lane_zt (0, zt_f16, 4);  /* { dg-error {passing 4 to argument 3 of 'svwrite_lane_zt', which expects a value in the range \[0, 3\]} }  */
  svwrite_lane_zt (0, zt_f32, 4);  /* { dg-error {passing 4 to argument 3 of 'svwrite_lane_zt', which expects a value in the range \[0, 3\]} }  */
  svwrite_lane_zt (0, zt_f64, 4);  /* { dg-error {passing 4 to argument 3 of 'svwrite_lane_zt', which expects a value in the range \[0, 3\]} }  */
}

#pragma GCC reset_options
#pragma GCC target("+sve2,+sme2")
void
test_svwrite_lane_zt_feature_not_enabled (svuint8_t zt_u8) __arm_streaming __arm_out ("zt0")
{
  // GCC only complains for the first such instance, so only one test here.
  svwrite_lane_zt (0, zt_u8, 0); /* { dg-error {ACLE function 'svwrite_lane_zt_u8' requires ISA extension 'sme-lutv2'} } */
}
