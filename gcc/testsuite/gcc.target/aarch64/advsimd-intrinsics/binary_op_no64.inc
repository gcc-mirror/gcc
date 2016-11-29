/* Can't use the standard binary_op.inc template because vmax has no
   64 bits variant.  */

#include <math.h>

#define FNNAME1(NAME) exec_ ## NAME
#define FNNAME(NAME) FNNAME1(NAME)

void FNNAME (INSN_NAME) (void)
{
  int i;

  /* Basic test: y=vmax(x,x), then store the result.  */
#define TEST_BINARY_OP1(INSN, Q, T1, T2, W, N)				\
  VECT_VAR(vector_res, T1, W, N) =                                      \
    INSN##Q##_##T2##W(VECT_VAR(vector, T1, W, N),                       \
                      VECT_VAR(vector2, T1, W, N));                     \
  vst1##Q##_##T2##W(VECT_VAR(result, T1, W, N), VECT_VAR(vector_res, T1, W, N))

#define TEST_BINARY_OP(INSN, Q, T1, T2, W, N)   \
  TEST_BINARY_OP1(INSN, Q, T1, T2, W, N)        \

  DECL_VARIABLE_ALL_VARIANTS(vector);
  DECL_VARIABLE_ALL_VARIANTS(vector2);
  DECL_VARIABLE_ALL_VARIANTS(vector_res);

  clean_results ();

  /* Initialize input "vector" from "buffer".  */
  TEST_MACRO_ALL_VARIANTS_2_5(VLOAD, vector, buffer);
#ifdef HAS_FLOAT16_VARIANT
  VLOAD(vector, buffer, , float, f, 16, 4);
  VLOAD(vector, buffer, q, float, f, 16, 8);
#endif
#ifdef HAS_FLOAT_VARIANT
  VLOAD(vector, buffer, , float, f, 32, 2);
  VLOAD(vector, buffer, q, float, f, 32, 4);
#endif

  /* Choose init value arbitrarily, will be used as comparison value.  */
  VDUP(vector2, , int, s, 8, 8, -13);
  VDUP(vector2, , int, s, 16, 4, -14);
  VDUP(vector2, , int, s, 32, 2, -16);
  VDUP(vector2, , uint, u, 8, 8, 0xf3);
  VDUP(vector2, , uint, u, 16, 4, 0xfff1);
  VDUP(vector2, , uint, u, 32, 2, 0xfffffff0);
  VDUP(vector2, q, int, s, 8, 16, -12);
  VDUP(vector2, q, int, s, 16, 8, -13);
  VDUP(vector2, q, int, s, 32, 4, -15);
  VDUP(vector2, q, uint, u, 8, 16, 0xf9);
  VDUP(vector2, q, uint, u, 16, 8, 0xfff2);
  VDUP(vector2, q, uint, u, 32, 4, 0xfffffff1);
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
  MACRO(VAR, , int, s, 8, 8);				\
  MACRO(VAR, , int, s, 16, 4);				\
  MACRO(VAR, , int, s, 32, 2);				\
  MACRO(VAR, , uint, u, 8, 8);				\
  MACRO(VAR, , uint, u, 16, 4);				\
  MACRO(VAR, , uint, u, 32, 2);				\
  MACRO(VAR, q, int, s, 8, 16);				\
  MACRO(VAR, q, int, s, 16, 8);				\
  MACRO(VAR, q, int, s, 32, 4);				\
  MACRO(VAR, q, uint, u, 8, 16);			\
  MACRO(VAR, q, uint, u, 16, 8);			\
  MACRO(VAR, q, uint, u, 32, 4);			\
  FLOAT_VARIANT(MACRO, VAR);				\
  FLOAT16_VARIANT(MACRO, VAR);

  /* Apply a binary operator named INSN_NAME.  */
  TEST_MACRO_NO64BIT_VARIANT_1_5(TEST_BINARY_OP, INSN_NAME);

  CHECK(TEST_MSG, int, 8, 8, PRIx8, expected, "");
  CHECK(TEST_MSG, int, 16, 4, PRIx16, expected, "");
  CHECK(TEST_MSG, int, 32, 2, PRIx32, expected, "");
  CHECK(TEST_MSG, uint, 8, 8, PRIx8, expected, "");
  CHECK(TEST_MSG, uint, 16, 4, PRIx16, expected, "");
  CHECK(TEST_MSG, uint, 32, 2, PRIx32, expected, "");
  CHECK(TEST_MSG, int, 8, 16, PRIx8, expected, "");
  CHECK(TEST_MSG, int, 16, 8, PRIx16, expected, "");
  CHECK(TEST_MSG, int, 32, 4, PRIx32, expected, "");
  CHECK(TEST_MSG, uint, 8, 16, PRIx8, expected, "");
  CHECK(TEST_MSG, uint, 16, 8, PRIx16, expected, "");
  CHECK(TEST_MSG, uint, 32, 4, PRIx32, expected, "");

#ifdef HAS_FLOAT16_VARIANT
  CHECK_FP(TEST_MSG, float, 16, 4, PRIx16, expected, "");
  CHECK_FP(TEST_MSG, float, 16, 8, PRIx16, expected, "");

  /* Extra FP tests with special values (NaN, ....)  */
  VDUP(vector, q, float, f, 16, 8, 1.0f);
  VDUP(vector2, q, float, f, 16, 8, NAN);
  TEST_BINARY_OP(INSN_NAME, q, float, f, 16, 8);
  CHECK_FP(TEST_MSG, float, 16, 8, PRIx16, expected_nan, " FP special (NaN)");

  VDUP(vector, q, float, f, 16, 8, -NAN);
  VDUP(vector2, q, float, f, 16, 8, 1.0f);
  TEST_BINARY_OP(INSN_NAME, q, float, f, 16, 8);
  CHECK_FP(TEST_MSG, float, 16, 8, PRIx16, expected_mnan, " FP special (-NaN)");

  VDUP(vector, q, float, f, 16, 8, 1.0f);
  VDUP(vector2, q, float, f, 16, 8, HUGE_VALF);
  TEST_BINARY_OP(INSN_NAME, q, float, f, 16, 8);
  CHECK_FP(TEST_MSG, float, 16, 8, PRIx16, expected_inf, " FP special (inf)");

  VDUP(vector, q, float, f, 16, 8, -HUGE_VALF);
  VDUP(vector2, q, float, f, 16, 8, 1.0f);
  TEST_BINARY_OP(INSN_NAME, q, float, f, 16, 8);
  CHECK_FP(TEST_MSG, float, 16, 8, PRIx16, expected_minf, " FP special (-inf)");

  VDUP(vector, q, float, f, 16, 8, 0.0f);
  VDUP(vector2, q, float, f, 16, 8, -0.0f);
  TEST_BINARY_OP(INSN_NAME, q, float, f, 16, 8);
  CHECK_FP(TEST_MSG, float, 16, 8, PRIx16, expected_zero1, " FP special (-0.0)");

  VDUP(vector, q, float, f, 16, 8, -0.0f);
  VDUP(vector2, q, float, f, 16, 8, 0.0f);
  TEST_BINARY_OP(INSN_NAME, q, float, f, 16, 8);
  CHECK_FP(TEST_MSG, float, 16, 8, PRIx16, expected_zero2, " FP special (-0.0)");
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
