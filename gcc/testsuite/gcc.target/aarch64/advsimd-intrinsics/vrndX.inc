#define FNNAME1(NAME) exec_ ## NAME
#define FNNAME(NAME) FNNAME1 (NAME)

void FNNAME (INSN) (void)
{
  /* vector_res = vrndX (vector), then store the result.  */
#define TEST_VRND2(INSN, Q, T1, T2, W, N)				\
  VECT_VAR (vector_res, T1, W, N) =					\
    INSN##Q##_##T2##W (VECT_VAR (vector, T1, W, N));			\
    vst1##Q##_##T2##W (VECT_VAR (result, T1, W, N),			\
		       VECT_VAR (vector_res, T1, W, N))

  /* Two auxliary macros are necessary to expand INSN.  */
#define TEST_VRND1(INSN, Q, T1, T2, W, N)	\
  TEST_VRND2 (INSN, Q, T1, T2, W, N)

#define TEST_VRND(Q, T1, T2, W, N)		\
  TEST_VRND1 (INSN, Q, T1, T2, W, N)

#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  DECL_VARIABLE(vector, float, 16, 4);
  DECL_VARIABLE(vector, float, 16, 8);
#endif
  DECL_VARIABLE (vector, float, 32, 2);
  DECL_VARIABLE (vector, float, 32, 4);

#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  DECL_VARIABLE(vector_res, float, 16, 4);
  DECL_VARIABLE(vector_res, float, 16, 8);
#endif
  DECL_VARIABLE (vector_res, float, 32, 2);
  DECL_VARIABLE (vector_res, float, 32, 4);

  clean_results ();

#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  VLOAD (vector, buffer, , float, f, 16, 4);
  VLOAD (vector, buffer, q, float, f, 16, 8);
#endif
  VLOAD (vector, buffer, , float, f, 32, 2);
  VLOAD (vector, buffer, q, float, f, 32, 4);

#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  TEST_VRND ( , float, f, 16, 4);
  TEST_VRND (q, float, f, 16, 8);
#endif
  TEST_VRND ( , float, f, 32, 2);
  TEST_VRND (q, float, f, 32, 4);

#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  CHECK_FP (TEST_MSG, float, 16, 4, PRIx16, expected, "");
  CHECK_FP (TEST_MSG, float, 16, 8, PRIx16, expected, "");
#endif
  CHECK_FP (TEST_MSG, float, 32, 2, PRIx32, expected, "");
  CHECK_FP (TEST_MSG, float, 32, 4, PRIx32, expected, "");
}

int
main (void)
{
  FNNAME (INSN) ();
  return 0;
}
