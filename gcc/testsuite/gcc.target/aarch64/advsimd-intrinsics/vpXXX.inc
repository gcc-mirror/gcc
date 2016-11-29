#define FNNAME1(NAME) exec_ ## NAME
#define FNNAME(NAME) FNNAME1(NAME)

void FNNAME (INSN_NAME) (void)
{
  /* Basic test: y=OP(x), then store the result.  */
#define TEST_VPXXX1(INSN, T1, T2, W, N)					\
  VECT_VAR(vector_res, T1, W, N) =					\
    INSN##_##T2##W(VECT_VAR(vector, T1, W, N),				\
		   VECT_VAR(vector, T1, W, N));				\
  vst1##_##T2##W(VECT_VAR(result, T1, W, N),				\
		 VECT_VAR(vector_res, T1, W, N))

#define TEST_VPXXX(INSN, T1, T2, W, N)		\
  TEST_VPXXX1(INSN, T1, T2, W, N)		\

  /* No need for 64 bits variants.  */
  DECL_VARIABLE(vector, int, 8, 8);
  DECL_VARIABLE(vector, int, 16, 4);
  DECL_VARIABLE(vector, int, 32, 2);
  DECL_VARIABLE(vector, uint, 8, 8);
  DECL_VARIABLE(vector, uint, 16, 4);
  DECL_VARIABLE(vector, uint, 32, 2);
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  DECL_VARIABLE(vector, float, 16, 4);
#endif
  DECL_VARIABLE(vector, float, 32, 2);

  DECL_VARIABLE(vector_res, int, 8, 8);
  DECL_VARIABLE(vector_res, int, 16, 4);
  DECL_VARIABLE(vector_res, int, 32, 2);
  DECL_VARIABLE(vector_res, uint, 8, 8);
  DECL_VARIABLE(vector_res, uint, 16, 4);
  DECL_VARIABLE(vector_res, uint, 32, 2);
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  DECL_VARIABLE(vector_res, float, 16, 4);
#endif
  DECL_VARIABLE(vector_res, float, 32, 2);

  clean_results ();

  /* Initialize input "vector" from "buffer".  */
  VLOAD(vector, buffer, , int, s, 8, 8);
  VLOAD(vector, buffer, , int, s, 16, 4);
  VLOAD(vector, buffer, , int, s, 32, 2);
  VLOAD(vector, buffer, , uint, u, 8, 8);
  VLOAD(vector, buffer, , uint, u, 16, 4);
  VLOAD(vector, buffer, , uint, u, 32, 2);
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  VLOAD(vector, buffer, , float, f, 16, 4);
#endif
  VLOAD(vector, buffer, , float, f, 32, 2);

  /* Apply a binary operator named INSN_NAME.  */
  TEST_VPXXX(INSN_NAME, int, s, 8, 8);
  TEST_VPXXX(INSN_NAME, int, s, 16, 4);
  TEST_VPXXX(INSN_NAME, int, s, 32, 2);
  TEST_VPXXX(INSN_NAME, uint, u, 8, 8);
  TEST_VPXXX(INSN_NAME, uint, u, 16, 4);
  TEST_VPXXX(INSN_NAME, uint, u, 32, 2);
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  TEST_VPXXX(INSN_NAME, float, f, 16, 4);
#endif
  TEST_VPXXX(INSN_NAME, float, f, 32, 2);

  CHECK(TEST_MSG, int, 8, 8, PRIx32, expected, "");
  CHECK(TEST_MSG, int, 16, 4, PRIx64, expected, "");
  CHECK(TEST_MSG, int, 32, 2, PRIx32, expected, "");
  CHECK(TEST_MSG, uint, 8, 8, PRIx32, expected, "");
  CHECK(TEST_MSG, uint, 16, 4, PRIx64, expected, "");
  CHECK(TEST_MSG, uint, 32, 2, PRIx32, expected, "");
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  CHECK_FP(TEST_MSG, float, 16, 4, PRIx16, expected, "");
#endif
  CHECK_FP(TEST_MSG, float, 32, 2, PRIx32, expected, "");
}

int main (void)
{
  FNNAME (INSN_NAME) ();
  return 0;
}
