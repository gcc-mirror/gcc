/* { dg-options "-std=c23" }  */
/* { dg-do compile }  */

#pragma GCC target "+sve2,+sme-lutv2"
static_assert (__ARM_FEATURE_SME_LUTv2 == 1);
#include <arm_sme.h>

/* LUTI4 (four registers, 8-bit)
   Variants are also available for: _u8
   svint8x4_t svluti4_zt_s8_x4 (uint64_t zt0, svuint8x2_t zn)
	      __arm_streaming __arm_in ("zt0");  */

void
test_svluti4_zt_ok (svuint8x2_t zn_u8) __arm_streaming __arm_in ("zt0")
{
  svluti4_zt_s8_x4 (0, zn_u8);
  svluti4_zt_u8_x4 (0, zn_u8);
}

void
test_svluti4_zt_not_streaming (svuint8x2_t zn_u8)
{
  svluti4_zt_s8_x4 (0, zn_u8); /* { dg-error {ACLE function 'svluti4_zt_s8_x4' can only be called when SME streaming mode is enabled} }  */
  svluti4_zt_u8_x4 (0, zn_u8); /* { dg-error {ACLE function 'svluti4_zt_u8_x4' can only be called when SME streaming mode is enabled} }  */
}

void
test_svluti4_zt_streaming_compatible (svuint8x2_t zn_u8) __arm_streaming_compatible
{
  svluti4_zt_s8_x4 (0, zn_u8); /* { dg-error {ACLE function 'svluti4_zt_s8_x4' can only be called when SME streaming mode is enabled} }  */
  svluti4_zt_u8_x4 (0, zn_u8); /* { dg-error {ACLE function 'svluti4_zt_u8_x4' can only be called when SME streaming mode is enabled} }  */
}

void
test_svluti4_zt_arg_count_mismatch (svuint8x2_t zn_u8) __arm_streaming __arm_in ("zt0")
{
  svluti4_zt_s8_x4 (); /* { dg-error {too few arguments to function 'svluti4_zt_s8_x4'; expected 2, have 0} }  */
  svluti4_zt_u8_x4 (); /* { dg-error {too few arguments to function 'svluti4_zt_u8_x4'; expected 2, have 0} }  */

  svluti4_zt_s8_x4 (0); /* { dg-error {too few arguments to function 'svluti4_zt_s8_x4'; expected 2, have 1} }  */
  svluti4_zt_u8_x4 (0); /* { dg-error {too few arguments to function 'svluti4_zt_u8_x4'; expected 2, have 1} }  */

  svluti4_zt_s8_x4 (0, zn_u8, 0); /* { dg-error {too many arguments to function 'svluti4_zt_s8_x4'; expected 2, have 3} }  */
  svluti4_zt_u8_x4 (0, zn_u8, 0); /* { dg-error {too many arguments to function 'svluti4_zt_u8_x4'; expected 2, have 3} }  */
}

void
test_svluti4_zt_arg_type_mismatch (svuint8x2_t zn_u8) __arm_streaming __arm_in ("zt0")
{
  struct Foo { uint64_t val; } foo = {0};
  svluti4_zt_s8_x4 (foo, zn_u8); /* { dg-error {incompatible type for argument 1 of 'svluti4_zt_s8_x4'} }  */
  svluti4_zt_u8_x4 (foo, zn_u8); /* { dg-error {incompatible type for argument 1 of 'svluti4_zt_u8_x4'} }  */
}

void
test_svluti4_zt_ret_type_mismatch (svuint8x2_t zn_u8) __arm_streaming __arm_in ("zt0")
{
  int x0 = svluti4_zt_s8_x4 (0, zn_u8); /* { dg-error {incompatible types when initializing type 'int' using type 'svint8x4_t'} }   */
  int x1 = svluti4_zt_u8_x4 (0, zn_u8); /* { dg-error {incompatible types when initializing type 'int' using type 'svuint8x4_t'} }  */
}

void
test_svluti4_zt_zt0_not_immediate (uint64_t zt0, svuint8x2_t zn_u8) __arm_streaming __arm_in ("zt0")
{
  svluti4_zt_s8_x4 (zt0, zn_u8); /* { dg-error {argument 1 of 'svluti4_zt_s8_x4' must be an integer constant expression} }  */
  svluti4_zt_u8_x4 (zt0, zn_u8); /* { dg-error {argument 1 of 'svluti4_zt_u8_x4' must be an integer constant expression} }  */
}

void
test_svluti4_zt_zt0_not_in_range (uint64_t zt0, svuint8x2_t zn_u8)__arm_streaming __arm_in ("zt0")
{
  svluti4_zt_s8_x4 (1, zn_u8); /* { dg-error {passing 1 to argument 1 of 'svluti4_zt_s8_x4', which expects the value 0} }  */
  svluti4_zt_u8_x4 (1, zn_u8); /* { dg-error {passing 1 to argument 1 of 'svluti4_zt_u8_x4', which expects the value 0} }  */

  svluti4_zt_s8_x4 (-1, zn_u8); /* { dg-error {passing -1 to argument 1 of 'svluti4_zt_s8_x4', which expects the value 0} }  */
  svluti4_zt_u8_x4 (-1, zn_u8); /* { dg-error {passing -1 to argument 1 of 'svluti4_zt_u8_x4', which expects the value 0} }  */
}

#pragma GCC reset_options
#pragma GCC target("+sve2,+sme2")
void
test_svluti4_zt_feature_not_enabled (svuint8x2_t zn_u8)__arm_streaming __arm_in ("zt0")
{
  // GCC only complains for the first such instance, so only one test here.
  svluti4_zt_s8_x4 (0, zn_u8); /* { dg-error {ACLE function 'svluti4_zt_s8_x4' requires ISA extension 'sme-lutv2'} } */
}
