#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

/* Expected results.  */
int8_t     expected_s8   = 0xf7;
int16_t    expected_s16  = 0xfff3;
int32_t    expected_s32  = 0xfffffff1;
int64_t    expected_s64  = 0xfffffffffffffff0;
uint8_t    expected_u8   = 0xf6;
uint16_t   expected_u16  = 0xfff2;
uint32_t   expected_u32  = 0xfffffff1;
uint64_t   expected_u64  = 0xfffffffffffffff0;
poly8_t    expected_p8   = 0xf6;
poly16_t   expected_p16  = 0xfff2;
hfloat16_t expected_f16  = 0xcb80;
hfloat32_t expected_f32  = 0xc1700000;

int8_t     expectedq_s8  = 0xff;
int16_t    expectedq_s16 = 0xfff5;
int32_t    expectedq_s32 = 0xfffffff3;
int64_t    expectedq_s64 = 0xfffffffffffffff1;
uint8_t    expectedq_u8  = 0xfe;
uint16_t   expectedq_u16 = 0xfff6;
uint32_t   expectedq_u32 = 0xfffffff2;
uint64_t   expectedq_u64 = 0xfffffffffffffff1;
poly8_t    expectedq_p8  = 0xfe;
poly16_t   expectedq_p16 = 0xfff6;
hfloat16_t expectedq_f16 = 0xca80;
hfloat32_t expectedq_f32 = 0xc1500000;

int error_found = 0;

#define TEST_MSG "VGET_LANE"
void exec_vget_lane (void)
{
  /* vec=vget_lane(vec, lane), then store the result.  */
#define TEST_VGET_LANE(Q, T1, T2, W, N, L)				   \
  VAR(var, T1, W) = vget##Q##_lane_##T2##W(VECT_VAR(vector, T1, W, N), L); \
  if (VAR(var, T1, W) != expected##Q##_##T2##W) {			   \
    fprintf(stderr,							   \
	    "ERROR in %s (%s line %d in result '%s') at type %s "	   \
	    "got 0x%" PRIx##W " != 0x%" PRIx##W "\n",			   \
	    TEST_MSG, __FILE__, __LINE__,				   \
	    STR(expected##Q##_##T2##W),					   \
	    STR(VECT_NAME(T1, W, N)),					   \
	    VAR(var, T1, W),						   \
	    expected##Q##_##T2##W);					   \
    error_found = 1;							   \
  }

  /* Special variant for floating-point.  */
  union {
    uint32_t var_int32;
    float32_t var_float32;
  } var_int32_float32;
  union {
    uint16_t var_int16;
    float16_t var_float16;
  } var_int16_float16;

#define TEST_VGET_LANE_FP(Q, T1, T2, W, N, L)				   \
  VAR(var, T1, W) = vget##Q##_lane_##T2##W(VECT_VAR(vector, T1, W, N), L); \
  var_int##W##_float##W.var_float##W = VAR(var, T1, W);			   \
  if (var_int##W##_float##W.var_int##W != expected##Q##_##T2##W) {	   \
    fprintf(stderr,							   \
	    "ERROR in %s (%s line %d in result '%s') at type %s "	   \
	    "got 0x%" PRIx##W " != 0x%" PRIx##W "\n",			   \
	    TEST_MSG, __FILE__, __LINE__,				   \
	    STR(expected##Q##_##T2##W),					   \
	    STR(VECT_NAME(T1, W, N)),					   \
	    var_int##W##_float##W.var_int##W,				   \
	    expected##Q##_##T2##W);					   \
    error_found = 1;							   \
  }

  DECL_VARIABLE_ALL_VARIANTS(vector);

  /* Scalar variables.  */
  VAR_DECL(var, int, 8);
  VAR_DECL(var, int, 16);
  VAR_DECL(var, int, 32);
  VAR_DECL(var, int, 64);
  VAR_DECL(var, uint, 8);
  VAR_DECL(var, uint, 16);
  VAR_DECL(var, uint, 32);
  VAR_DECL(var, uint, 64);
  VAR_DECL(var, poly, 8);
  VAR_DECL(var, poly, 16);
#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
  VAR_DECL(var, float, 16);
#endif
  VAR_DECL(var, float, 32);

  /* Initialize input values.  */
  TEST_MACRO_ALL_VARIANTS_2_5(VLOAD, vector, buffer);
#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
  VLOAD(vector, buffer, , float, f, 16, 4);
  VLOAD(vector, buffer, q, float, f, 16, 8);
#endif
  VLOAD(vector, buffer, , float, f, 32, 2);
  VLOAD(vector, buffer, q, float, f, 32, 4);

  /* Choose lane arbitrarily.  */
  TEST_VGET_LANE(, int, s, 8, 8, 7);
  TEST_VGET_LANE(, int, s, 16, 4, 3);
  TEST_VGET_LANE(, int, s, 32, 2, 1);
  TEST_VGET_LANE(, int, s, 64, 1, 0);
  TEST_VGET_LANE(, uint, u, 8, 8, 6);
  TEST_VGET_LANE(, uint, u, 16, 4, 2);
  TEST_VGET_LANE(, uint, u, 32, 2, 1);
  TEST_VGET_LANE(, uint, u, 64, 1, 0);
  TEST_VGET_LANE(, poly, p, 8, 8, 6);
  TEST_VGET_LANE(, poly, p, 16, 4, 2);
#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
  TEST_VGET_LANE_FP(, float, f, 16, 4, 1);
#endif
  TEST_VGET_LANE_FP(, float, f, 32, 2, 1);

  TEST_VGET_LANE(q, int, s, 8, 16, 15);
  TEST_VGET_LANE(q, int, s, 16, 8, 5);
  TEST_VGET_LANE(q, int, s, 32, 4, 3);
  TEST_VGET_LANE(q, int, s, 64, 2, 1);
  TEST_VGET_LANE(q, uint, u, 8, 16, 14);
  TEST_VGET_LANE(q, uint, u, 16, 8, 6);
  TEST_VGET_LANE(q, uint, u, 32, 4, 2);
  TEST_VGET_LANE(q, uint, u, 64, 2, 1);
  TEST_VGET_LANE(q, poly, p, 8, 16, 14);
  TEST_VGET_LANE(q, poly, p, 16, 8, 6);
#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
  TEST_VGET_LANE_FP(q, float, f, 16, 8, 3);
#endif
  TEST_VGET_LANE_FP(q, float, f, 32, 4, 3);
}

int main (void)
{
  exec_vget_lane ();

  if (error_found)
    abort();

  return 0;
}
