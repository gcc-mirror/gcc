#define FNNAME1(NAME) exec_ ## NAME
#define FNNAME(NAME) FNNAME1(NAME)

void FNNAME (INSN_NAME) (void)
{
#define DECL_VMLX_N(VAR)			\
  DECL_VARIABLE(VAR, int, 16, 4);		\
  DECL_VARIABLE(VAR, int, 32, 2);		\
  DECL_VARIABLE(VAR, uint, 16, 4);		\
  DECL_VARIABLE(VAR, uint, 32, 2);		\
  DECL_VARIABLE(VAR, float, 32, 2);		\
  DECL_VARIABLE(VAR, int, 16, 8);		\
  DECL_VARIABLE(VAR, int, 32, 4);		\
  DECL_VARIABLE(VAR, uint, 16, 8);		\
  DECL_VARIABLE(VAR, float, 32, 4);		\
  DECL_VARIABLE(VAR, uint, 32, 4)

  /* vector_res = vmlx_n(vector, vector2, val),
     then store the result.  */
#define TEST_VMLX_N1(INSN, Q, T1, T2, W, N, V)          \
  VECT_VAR(vector_res, T1, W, N) =                      \
    INSN##Q##_n_##T2##W(VECT_VAR(vector, T1, W, N),     \
			VECT_VAR(vector2, T1, W, N),    \
			V);                             \
  vst1##Q##_##T2##W(VECT_VAR(result, T1, W, N),         \
		    VECT_VAR(vector_res, T1, W, N))

#define TEST_VMLX_N(INSN, Q, T1, T2, W, N, V)	\
  TEST_VMLX_N1(INSN, Q, T1, T2, W, N, V)

  DECL_VMLX_N(vector);
  DECL_VMLX_N(vector2);
  DECL_VMLX_N(vector_res);

  clean_results ();

  VLOAD(vector, buffer, , int, s, 16, 4);
  VLOAD(vector, buffer, , int, s, 32, 2);
  VLOAD(vector, buffer, , uint, u, 16, 4);
  VLOAD(vector, buffer, , uint, u, 32, 2);
  VLOAD(vector, buffer, , float, f, 32, 2);
  VLOAD(vector, buffer, q, int, s, 16, 8);
  VLOAD(vector, buffer, q, int, s, 32, 4);
  VLOAD(vector, buffer, q, uint, u, 16, 8);
  VLOAD(vector, buffer, q, uint, u, 32, 4);
  VLOAD(vector, buffer, q, float, f, 32, 4);

  VDUP(vector2, , int, s, 16, 4, 0x55);
  VDUP(vector2, , int, s, 32, 2, 0x55);
  VDUP(vector2, , uint, u, 16, 4, 0x55);
  VDUP(vector2, , uint, u, 32, 2, 0x55);
  VDUP(vector2, , float, f, 32, 2, 55.2f);
  VDUP(vector2, q, int, s, 16, 8, 0x55);
  VDUP(vector2, q, int, s, 32, 4, 0x55);
  VDUP(vector2, q, uint, u, 16, 8, 0x55);
  VDUP(vector2, q, uint, u, 32, 4, 0x55);
  VDUP(vector2, q, float, f, 32, 4, 55.9f);

  /* Choose multiplier arbitrarily.  */
  TEST_VMLX_N(INSN_NAME, , int, s, 16, 4, 0x11);
  TEST_VMLX_N(INSN_NAME, , int, s, 32, 2, 0x22);
  TEST_VMLX_N(INSN_NAME, , uint, u, 16, 4, 0x33);
  TEST_VMLX_N(INSN_NAME, , uint, u, 32, 2, 0x44);
  TEST_VMLX_N(INSN_NAME, , float, f, 32, 2, 22.3f);
  TEST_VMLX_N(INSN_NAME, q, int, s, 16, 8, 0x55);
  TEST_VMLX_N(INSN_NAME, q, int, s, 32, 4, 0x66);
  TEST_VMLX_N(INSN_NAME, q, uint, u, 16, 8, 0x77);
  TEST_VMLX_N(INSN_NAME, q, uint, u, 32, 4, 0x88);
  TEST_VMLX_N(INSN_NAME, q, float, f, 32, 4, 66.7f);

  CHECK(TEST_MSG, int, 16, 4, PRIx16, expected, "");
  CHECK(TEST_MSG, int, 32, 2, PRIx32, expected, "");
  CHECK(TEST_MSG, uint, 16, 4, PRIx16, expected, "");
  CHECK(TEST_MSG, uint, 32, 2, PRIx32, expected, "");
  CHECK_FP(TEST_MSG, float, 32, 2, PRIx32, expected, "");
  CHECK(TEST_MSG, int, 16, 8, PRIx16, expected, "");
  CHECK(TEST_MSG, int, 32, 4, PRIx32, expected, "");
  CHECK(TEST_MSG, uint, 16, 8, PRIx16, expected, "");
  CHECK(TEST_MSG, uint, 32, 4, PRIx32, expected, "");
  CHECK_FP(TEST_MSG, float, 32, 4, PRIx32, expected, "");
}

int main (void)
{
  FNNAME (INSN_NAME) ();
  return 0;
}
