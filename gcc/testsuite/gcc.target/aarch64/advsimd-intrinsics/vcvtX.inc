/* Template file for VCVT operator validation.

   This file is meant to be included by the relevant test files, which
   have to define the intrinsic family to test.  If a given intrinsic
   supports variants which are not supported by all the other vcvt
   operators, these can be tested by providing a definition for
   EXTRA_TESTS.

   This file is only used for VCVT? tests, which currently have only f16 to
   integer variants.  It is based on vcvt.c.  */

#define FNNAME1(NAME) exec_ ## NAME
#define FNNAME(NAME) FNNAME1 (NAME)

void FNNAME (INSN_NAME) (void)
{
  int i;

  /* Basic test: y=vcvt(x), then store the result.  */
#define TEST_VCVT1(INSN, Q, T1, T2, W, N, TS1, TS2, EXP)	\
  VECT_VAR(vector_res, T1, W, N) =				\
    INSN##Q##_##T2##W##_##TS2##W(VECT_VAR(vector, TS1, W, N));	\
  vst1##Q##_##T2##W(VECT_VAR(result, T1, W, N),			\
		    VECT_VAR(vector_res, T1, W, N));		\
  CHECK(TEST_MSG, T1, W, N, PRIx##W, EXP, TEST_MSG2);

#define TEST_VCVT(INSN, Q, T1, T2, W, N, TS1, TS2, EXP)		\
  TEST_VCVT1 (INSN, Q, T1, T2, W, N, TS1, TS2, EXP)

  DECL_VARIABLE_ALL_VARIANTS(vector);
  DECL_VARIABLE_ALL_VARIANTS(vector_res);

  clean_results ();

  /* Initialize input "vector" from "buffer".  */
  TEST_MACRO_ALL_VARIANTS_2_5(VLOAD, vector, buffer);
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  VLOAD(vector, buffer, , float, f, 16, 4);
  VLOAD(vector, buffer, q, float, f, 16, 8);
#endif

  /* Make sure some elements have a fractional part, to exercise
     integer conversions.  */
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  VSET_LANE(vector, , float, f, 16, 4, 0, -15.3f);
  VSET_LANE(vector, , float, f, 16, 4, 1, 5.3f);
  VSET_LANE(vector, , float, f, 16, 4, 2, -15.3f);
  VSET_LANE(vector, , float, f, 16, 4, 3, 5.3f);
  VSET_LANE(vector, q, float, f, 16, 8, 4, -15.3f);
  VSET_LANE(vector, q, float, f, 16, 8, 5, 5.3f);
  VSET_LANE(vector, q, float, f, 16, 8, 6, -15.3f);
  VSET_LANE(vector, q, float, f, 16, 8, 7, 5.3f);
#endif

  /* The same result buffers are used multiple times, so we check them
     before overwriting them.  */
#define TEST_MSG2 ""

#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  /* vcvt?_xx_f16.  */
  TEST_VCVT(INSN_NAME, , int, s, 16, 4, float, f, expected);
  TEST_VCVT(INSN_NAME, , uint, u, 16, 4, float, f, expected);
#endif

#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  VSET_LANE(vector, q, float, f, 16, 8, 0, 0.0f);
  VSET_LANE(vector, q, float, f, 16, 8, 1, -0.0f);
  VSET_LANE(vector, q, float, f, 16, 8, 2, 15.12f);
  VSET_LANE(vector, q, float, f, 16, 8, 3, -15.12f);
  VSET_LANE(vector, q, float, f, 16, 8, 4, 0.0f);
  VSET_LANE(vector, q, float, f, 16, 8, 5, -0.0f);
  VSET_LANE(vector, q, float, f, 16, 8, 6, 15.12f);
  VSET_LANE(vector, q, float, f, 16, 8, 7, -15.12f);
#endif

#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  /* vcvt?q_xx_f16.  */
  TEST_VCVT(INSN_NAME, q, int, s, 16, 8, float, f, expected);
  TEST_VCVT(INSN_NAME, q, uint, u, 16, 8, float, f, expected);
#endif

  /* Check rounding.  */
#undef TEST_MSG2
#define TEST_MSG2 "(check rounding)"

#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  VDUP(vector, , float, f, 16, 4, 10.4f);
  VDUP(vector, q, float, f, 16, 8, 125.9f);
#endif

#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  /* vcvt?_xx_f16.  */
  TEST_VCVT(INSN_NAME, , int, s, 16, 4, float, f, expected_rounding);
  TEST_VCVT(INSN_NAME, , uint, u, 16, 4, float, f, expected_rounding);
#endif

#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  /* vcvt?q_xx_f16.  */
  TEST_VCVT(INSN_NAME, q, int, s, 16, 8, float, f, expected_rounding);
  TEST_VCVT(INSN_NAME, q, uint, u, 16, 8, float, f, expected_rounding);
#endif

#ifdef EXTRA_TESTS
  EXTRA_TESTS();
#endif
}

int
main (void)
{
  FNNAME (INSN_NAME) ();
  return 0;
}
