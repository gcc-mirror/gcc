#define FNNAME1(NAME) exec_ ## NAME
#define FNNAME(NAME) FNNAME1(NAME)

void FNNAME (INSN_NAME) (void)
{
  /* Basic test: y=vaddw(x1,x2), then store the result.  */
#define TEST_VADDW1(INSN, T1, T2, W, W2, N)				\
  VECT_VAR(vector_res, T1, W2, N) =					\
    INSN##_##T2##W(VECT_VAR(vector, T1, W2, N),				\
		   VECT_VAR(vector2, T1, W, N));			\
  vst1q_##T2##W2(VECT_VAR(result, T1, W2, N), VECT_VAR(vector_res, T1, W2, N))

#define TEST_VADDW(INSN, T1, T2, W, W2, N)	\
  TEST_VADDW1(INSN, T1, T2, W, W2, N)

  DECL_VARIABLE(vector, int, 16, 8);
  DECL_VARIABLE(vector, int, 32, 4);
  DECL_VARIABLE(vector, int, 64, 2);
  DECL_VARIABLE(vector, uint, 16, 8);
  DECL_VARIABLE(vector, uint, 32, 4);
  DECL_VARIABLE(vector, uint, 64, 2);

  DECL_VARIABLE(vector2, int, 8, 8);
  DECL_VARIABLE(vector2, int, 16, 4);
  DECL_VARIABLE(vector2, int, 32, 2);
  DECL_VARIABLE(vector2, uint, 8, 8);
  DECL_VARIABLE(vector2, uint, 16, 4);
  DECL_VARIABLE(vector2, uint, 32, 2);

  DECL_VARIABLE(vector_res, int, 16, 8);
  DECL_VARIABLE(vector_res, int, 32, 4);
  DECL_VARIABLE(vector_res, int, 64, 2);
  DECL_VARIABLE(vector_res, uint, 16, 8);
  DECL_VARIABLE(vector_res, uint, 32, 4);
  DECL_VARIABLE(vector_res, uint, 64, 2);

  clean_results ();

  /* Initialize input "vector" from "buffer".  */
  VLOAD(vector, buffer, q, int, s, 16, 8);
  VLOAD(vector, buffer, q, int, s, 32, 4);
  VLOAD(vector, buffer, q, int, s, 64, 2);
  VLOAD(vector, buffer, q, uint, u, 16, 8);
  VLOAD(vector, buffer, q, uint, u, 32, 4);
  VLOAD(vector, buffer, q, uint, u, 64, 2);

  /* Choose init value arbitrarily.  */
  VDUP(vector2, , int, s, 8, 8, -13);
  VDUP(vector2, , int, s, 16, 4, -14);
  VDUP(vector2, , int, s, 32, 2, -16);
  VDUP(vector2, , uint, u, 8, 8, 0xf3);
  VDUP(vector2, , uint, u, 16, 4, 0xfff1);
  VDUP(vector2, , uint, u, 32, 2, 0xfffffff0);

  /* Execute the tests.  */
  TEST_VADDW(INSN_NAME, int, s, 8, 16, 8);
  TEST_VADDW(INSN_NAME, int, s, 16, 32, 4);
  TEST_VADDW(INSN_NAME, int, s, 32, 64, 2);
  TEST_VADDW(INSN_NAME, uint, u, 8, 16, 8);
  TEST_VADDW(INSN_NAME, uint, u, 16, 32, 4);
  TEST_VADDW(INSN_NAME, uint, u, 32, 64, 2);

  CHECK(TEST_MSG, int, 16, 8, PRIx16, expected, "");
  CHECK(TEST_MSG, int, 32, 4, PRIx32, expected, "");
  CHECK(TEST_MSG, int, 64, 2, PRIx64, expected, "");
  CHECK(TEST_MSG, uint, 16, 8, PRIx16, expected, "");
  CHECK(TEST_MSG, uint, 32, 4, PRIx32, expected, "");
  CHECK(TEST_MSG, uint, 64, 2, PRIx64, expected, "");
}

int main (void)
{
  FNNAME (INSN_NAME) ();
  return 0;
}
