#define FNNAME1(NAME) exec_ ## NAME
#define FNNAME(NAME) FNNAME1(NAME)

void FNNAME (INSN_NAME) (void)
{
  /* Basic test: y=OP(x), then store the result.  */
#define TEST_VPXXXQ1(INSN, T1, T2, W, N)				\
  VECT_VAR(vector_res, T1, W, N) =					\
    INSN##_##T2##W(VECT_VAR(vector, T1, W, N),				\
		   VECT_VAR(vector, T1, W, N));				\
  vst1q##_##T2##W(VECT_VAR(result, T1, W, N),				\
		  VECT_VAR(vector_res, T1, W, N))

#define TEST_VPXXXQ(INSN, T1, T2, W, N)					\
  TEST_VPXXXQ1(INSN, T1, T2, W, N)					\

  DECL_VARIABLE(vector, int, 8, 16);
  DECL_VARIABLE(vector, int, 16, 8);
  DECL_VARIABLE(vector, int, 32, 4);
  DECL_VARIABLE(vector, int, 64, 2);
  DECL_VARIABLE(vector, uint, 8, 16);
  DECL_VARIABLE(vector, uint, 16, 8);
  DECL_VARIABLE(vector, uint, 32, 4);
  DECL_VARIABLE(vector, uint, 64, 2);
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  DECL_VARIABLE(vector, float, 16, 8);
#endif
  DECL_VARIABLE(vector, float, 32, 4);
  DECL_VARIABLE(vector, float, 64, 2);

  DECL_VARIABLE(vector_res, int, 8, 16);
  DECL_VARIABLE(vector_res, int, 16, 8);
  DECL_VARIABLE(vector_res, int, 32, 4);
  DECL_VARIABLE(vector_res, int, 64, 2);
  DECL_VARIABLE(vector_res, uint, 8, 16);
  DECL_VARIABLE(vector_res, uint, 16, 8);
  DECL_VARIABLE(vector_res, uint, 32, 4);
  DECL_VARIABLE(vector_res, uint, 64, 2);
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  DECL_VARIABLE(vector_res, float, 16, 8);
#endif
  DECL_VARIABLE(vector_res, float, 32, 4);
  DECL_VARIABLE(vector_res, float, 64, 2);

  clean_results ();

  /* Initialize input "vector" from "buffer".  */
  VLOAD(vector, buffer, q, int, s, 8, 16);
  VLOAD(vector, buffer, q, int, s, 16, 8);
  VLOAD(vector, buffer, q, int, s, 32, 4);
  VLOAD(vector, buffer, q, int, s, 64, 2);
  VLOAD(vector, buffer, q, uint, u, 8, 16);
  VLOAD(vector, buffer, q, uint, u, 16, 8);
  VLOAD(vector, buffer, q, uint, u, 32, 4);
  VLOAD(vector, buffer, q, uint, u, 64, 2);
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  VLOAD(vector, buffer, q, float, f, 16, 8);
#endif
  VLOAD(vector, buffer, q, float, f, 32, 4);
  VLOAD(vector, buffer, q, float, f, 64, 2);

  /* Apply a binary operator named INSN_NAME.  */
  TEST_VPXXXQ(INSN_NAME, int, s, 8, 16);
  TEST_VPXXXQ(INSN_NAME, int, s, 16, 8);
  TEST_VPXXXQ(INSN_NAME, int, s, 32, 4);
  TEST_VPXXXQ(INSN_NAME, int, s, 64, 2);
  TEST_VPXXXQ(INSN_NAME, uint, u, 8, 16);
  TEST_VPXXXQ(INSN_NAME, uint, u, 16, 8);
  TEST_VPXXXQ(INSN_NAME, uint, u, 32, 4);
  TEST_VPXXXQ(INSN_NAME, uint, u, 64, 2);
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  TEST_VPXXXQ(INSN_NAME, float, f, 16, 8);
#endif
  TEST_VPXXXQ(INSN_NAME, float, f, 32, 4);
  TEST_VPXXXQ(INSN_NAME, float, f, 64, 2);

  CHECK(TEST_MSG, int, 8, 16, PRIx8, expected, "");
  CHECK(TEST_MSG, int, 16, 8, PRIx16, expected, "");
  CHECK(TEST_MSG, int, 32, 4, PRIx32, expected, "");
  CHECK(TEST_MSG, int, 64, 2, PRIx64, expected, "");
  CHECK(TEST_MSG, uint, 8, 16, PRIx8, expected, "");
  CHECK(TEST_MSG, uint, 16, 8, PRIx16, expected, "");
  CHECK(TEST_MSG, uint, 32, 4, PRIx32, expected, "");
  CHECK(TEST_MSG, uint, 64, 2, PRIx64, expected, "");
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  CHECK_FP(TEST_MSG, float, 16, 8, PRIx16, expected, "");
#endif
  CHECK_FP(TEST_MSG, float, 32, 4, PRIx32, expected, "");
  CHECK_FP(TEST_MSG, float, 64, 2, PRIx64, expected, "");
}

int main (void)
{
  FNNAME (INSN_NAME) ();
  return 0;
}
