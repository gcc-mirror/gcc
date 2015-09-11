#define FNNAME1(NAME) exec_ ## NAME
#define FNNAME(NAME) FNNAME1(NAME)

void FNNAME (INSN_NAME) (void)
{
  /* vector_res = vmlxl_n(vector, vector2, val),
     then store the result.  */
#define TEST_VMLXL_N1(INSN, T1, T2, W, W2, N, V)			\
  VECT_VAR(vector_res, T1, W, N) = INSN##_##T2##W2(VECT_VAR(vector, T1, W, N), \
						   VECT_VAR(vector2, T1, W2, N), \
						   V);			\
  vst1q_##T2##W(VECT_VAR(result, T1, W, N), VECT_VAR(vector_res, T1, W, N))

#define TEST_VMLXL_N(INSN, T1, T2, W, W2, N, V)			\
  TEST_VMLXL_N1(INSN, T1, T2, W, W2, N, V)

  DECL_VARIABLE(vector, int, 32, 4);
  DECL_VARIABLE(vector2, int, 16, 4);
  DECL_VARIABLE(vector_res, int, 32, 4);

  DECL_VARIABLE(vector, int, 64, 2);
  DECL_VARIABLE(vector2, int, 32, 2);
  DECL_VARIABLE(vector_res, int, 64, 2);

  DECL_VARIABLE(vector, uint, 32, 4);
  DECL_VARIABLE(vector2, uint, 16, 4);
  DECL_VARIABLE(vector_res, uint, 32, 4);

  DECL_VARIABLE(vector, uint, 64, 2);
  DECL_VARIABLE(vector2, uint, 32, 2);
  DECL_VARIABLE(vector_res, uint, 64, 2);

  clean_results ();

  VLOAD(vector, buffer, q, int, s, 32, 4);
  VLOAD(vector, buffer, q, int, s, 64, 2);
  VLOAD(vector, buffer, q, uint, u, 32, 4);
  VLOAD(vector, buffer, q, uint, u, 64, 2);

  VDUP(vector2, , int, s, 16, 4, 0x55);
  VDUP(vector2, , int, s, 32, 2, 0x55);
  VDUP(vector2, , uint, u, 16, 4, 0x55);
  VDUP(vector2, , uint, u, 32, 2, 0x55);

  /* Choose multiplier arbitrarily.  */
  TEST_VMLXL_N(INSN_NAME, int, s, 32, 16, 4, 0x11);
  TEST_VMLXL_N(INSN_NAME, int, s, 64, 32, 2, 0x22);
  TEST_VMLXL_N(INSN_NAME, uint, u, 32, 16, 4, 0x33);
  TEST_VMLXL_N(INSN_NAME, uint, u, 64, 32, 2, 0x33);

  CHECK(TEST_MSG, int, 32, 4, PRIx32, expected, "");
  CHECK(TEST_MSG, int, 64, 2, PRIx64, expected, "");
  CHECK(TEST_MSG, uint, 32, 4, PRIx32, expected, "");
  CHECK(TEST_MSG, uint, 64, 2, PRIx64, expected, "");
}

int main (void)
{
  FNNAME (INSN_NAME) ();
  return 0;
}
