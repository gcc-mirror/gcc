/* Floating-point only version of binary_op_no64.inc template.  Currently only
   float16_t is used.  */

#include <math.h>

#define FNNAME1(NAME) exec_ ## NAME
#define FNNAME(NAME) FNNAME1(NAME)

void FNNAME (INSN_NAME) (void)
{
  int i;

  /* Basic test: z = INSN (x, y), then store the result.  */
#define TEST_BINARY_OP1(INSN, Q, T1, T2, W, N)				\
  VECT_VAR(vector_res, T1, W, N) =					\
    INSN##Q##_##T2##W(VECT_VAR(vector, T1, W, N),			\
		      VECT_VAR(vector2, T1, W, N));			\
  vst1##Q##_##T2##W(VECT_VAR(result, T1, W, N), VECT_VAR(vector_res, T1, W, N))

#define TEST_BINARY_OP(INSN, Q, T1, T2, W, N)   \
  TEST_BINARY_OP1(INSN, Q, T1, T2, W, N)	\

#ifdef HAS_FLOAT16_VARIANT
  DECL_VARIABLE(vector, float, 16, 4);
  DECL_VARIABLE(vector2, float, 16, 4);
  DECL_VARIABLE(vector_res, float, 16, 4);

  DECL_VARIABLE(vector, float, 16, 8);
  DECL_VARIABLE(vector2, float, 16, 8);
  DECL_VARIABLE(vector_res, float, 16, 8);
#endif

#ifdef HAS_FLOAT_VARIANT
  DECL_VARIABLE(vector, float, 32, 2);
  DECL_VARIABLE(vector2, float, 32, 2);
  DECL_VARIABLE(vector_res, float, 32, 2);

  DECL_VARIABLE(vector, float, 32, 4);
  DECL_VARIABLE(vector2, float, 32, 4);
  DECL_VARIABLE(vector_res, float, 32, 4);
#endif

  clean_results ();

  /* Initialize input "vector" from "buffer".  */
#ifdef HAS_FLOAT16_VARIANT
  VLOAD(vector, buffer, , float, f, 16, 4);
  VLOAD(vector, buffer, q, float, f, 16, 8);
#endif
#ifdef HAS_FLOAT_VARIANT
  VLOAD(vector, buffer, , float, f, 32, 2);
  VLOAD(vector, buffer, q, float, f, 32, 4);
#endif

  /* Choose init value arbitrarily, will be used as comparison value.  */
#ifdef HAS_FLOAT16_VARIANT
  VDUP(vector2, , float, f, 16, 4, -15.5f);
  VDUP(vector2, q, float, f, 16, 8, -14.5f);
#endif
#ifdef HAS_FLOAT_VARIANT
  VDUP(vector2, , float, f, 32, 2, -15.5f);
  VDUP(vector2, q, float, f, 32, 4, -14.5f);
#endif

#ifdef HAS_FLOAT16_VARIANT
#define FLOAT16_VARIANT(MACRO, VAR)			\
  MACRO(VAR, , float, f, 16, 4);			\
  MACRO(VAR, q, float, f, 16, 8);
#else
#define FLOAT16_VARIANT(MACRO, VAR)
#endif

#ifdef HAS_FLOAT_VARIANT
#define FLOAT_VARIANT(MACRO, VAR)			\
  MACRO(VAR, , float, f, 32, 2);			\
  MACRO(VAR, q, float, f, 32, 4);
#else
#define FLOAT_VARIANT(MACRO, VAR)
#endif

#define TEST_MACRO_NO64BIT_VARIANT_1_5(MACRO, VAR)	\

  /* Apply a binary operator named INSN_NAME.  */
  FLOAT16_VARIANT(TEST_BINARY_OP, INSN_NAME);
  FLOAT_VARIANT(TEST_BINARY_OP, INSN_NAME);

#ifdef HAS_FLOAT16_VARIANT
  CHECK_FP(TEST_MSG, float, 16, 4, PRIx16, expected, "");
  CHECK_FP(TEST_MSG, float, 16, 8, PRIx16, expected, "");

  /* Extra FP tests with special values (NaN, ....)  */
  VDUP(vector, q, float, f, 16, 8, 1.0f);
  VDUP(vector2, q, float, f, 16, 8, NAN);
  TEST_BINARY_OP(INSN_NAME, q, float, f, 16, 8);
  CHECK_FP(TEST_MSG, float, 16, 8, PRIx16, expected_nan,
	   " FP special (NaN)");

  VDUP(vector, q, float, f, 16, 8, -NAN);
  VDUP(vector2, q, float, f, 16, 8, 1.0f);
  TEST_BINARY_OP(INSN_NAME, q, float, f, 16, 8);
  CHECK_FP(TEST_MSG, float, 16, 8, PRIx16, expected_mnan,
	   " FP special (-NaN)");

  VDUP(vector, q, float, f, 16, 8, 1.0f);
  VDUP(vector2, q, float, f, 16, 8, HUGE_VALF);
  TEST_BINARY_OP(INSN_NAME, q, float, f, 16, 8);
  CHECK_FP(TEST_MSG, float, 16, 8, PRIx16, expected_inf,
	   " FP special (inf)");

  VDUP(vector, q, float, f, 16, 8, -HUGE_VALF);
  VDUP(vector2, q, float, f, 16, 8, 1.0f);
  TEST_BINARY_OP(INSN_NAME, q, float, f, 16, 8);
  CHECK_FP(TEST_MSG, float, 16, 8, PRIx16, expected_minf,
	   " FP special (-inf)");

  VDUP(vector, q, float, f, 16, 8, 0.0f);
  VDUP(vector2, q, float, f, 16, 8, -0.0f);
  TEST_BINARY_OP(INSN_NAME, q, float, f, 16, 8);
  CHECK_FP(TEST_MSG, float, 16, 8, PRIx16, expected_zero1,
	   " FP special (-0.0)");

  VDUP(vector, q, float, f, 16, 8, -0.0f);
  VDUP(vector2, q, float, f, 16, 8, 0.0f);
  TEST_BINARY_OP(INSN_NAME, q, float, f, 16, 8);
  CHECK_FP(TEST_MSG, float, 16, 8, PRIx16, expected_zero2,
	   " FP special (-0.0)");
#endif

#ifdef HAS_FLOAT_VARIANT
  CHECK_FP(TEST_MSG, float, 32, 2, PRIx32, expected, "");
  CHECK_FP(TEST_MSG, float, 32, 4, PRIx32, expected, "");

  /* Extra FP tests with special values (NaN, ....)  */
  VDUP(vector, q, float, f, 32, 4, 1.0f);
  VDUP(vector2, q, float, f, 32, 4, NAN);
  TEST_BINARY_OP(INSN_NAME, q, float, f, 32, 4);
  CHECK_FP(TEST_MSG, float, 32, 4, PRIx32, expected_nan, " FP special (NaN)");

  VDUP(vector, q, float, f, 32, 4, -NAN);
  VDUP(vector2, q, float, f, 32, 4, 1.0f);
  TEST_BINARY_OP(INSN_NAME, q, float, f, 32, 4);
  CHECK_FP(TEST_MSG, float, 32, 4, PRIx32, expected_mnan, " FP special (-NaN)");

  VDUP(vector, q, float, f, 32, 4, 1.0f);
  VDUP(vector2, q, float, f, 32, 4, HUGE_VALF);
  TEST_BINARY_OP(INSN_NAME, q, float, f, 32, 4);
  CHECK_FP(TEST_MSG, float, 32, 4, PRIx32, expected_inf, " FP special (inf)");

  VDUP(vector, q, float, f, 32, 4, -HUGE_VALF);
  VDUP(vector2, q, float, f, 32, 4, 1.0f);
  TEST_BINARY_OP(INSN_NAME, q, float, f, 32, 4);
  CHECK_FP(TEST_MSG, float, 32, 4, PRIx32, expected_minf, " FP special (-inf)");

  VDUP(vector, q, float, f, 32, 4, 0.0f);
  VDUP(vector2, q, float, f, 32, 4, -0.0f);
  TEST_BINARY_OP(INSN_NAME, q, float, f, 32, 4);
  CHECK_FP(TEST_MSG, float, 32, 4, PRIx32, expected_zero1, " FP special (-0.0)");

  VDUP(vector, q, float, f, 32, 4, -0.0f);
  VDUP(vector2, q, float, f, 32, 4, 0.0f);
  TEST_BINARY_OP(INSN_NAME, q, float, f, 32, 4);
  CHECK_FP(TEST_MSG, float, 32, 4, PRIx32, expected_zero2, " FP special (-0.0)");
#endif
}

int main (void)
{
  FNNAME (INSN_NAME) ();
  return 0;
}
