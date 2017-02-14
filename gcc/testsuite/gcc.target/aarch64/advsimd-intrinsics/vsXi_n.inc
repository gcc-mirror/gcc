#define FNNAME1(NAME) exec_ ## NAME ##_n
#define FNNAME(NAME) FNNAME1(NAME)

void FNNAME (INSN_NAME) (void)
{
  /* vector_res = vsxi_n(vector, vector2, val),
     then store the result.  */
#define TEST_VSXI_N1(INSN, Q, T1, T2, W, N, V)				\
  VECT_VAR(vector_res, T1, W, N) =					\
    INSN##Q##_n_##T2##W(VECT_VAR(vector, T1, W, N),			\
			VECT_VAR(vector2, T1, W, N),			\
			V);						\
  vst1##Q##_##T2##W(VECT_VAR(result, T1, W, N), VECT_VAR(vector_res, T1, W, N))

#define TEST_VSXI_N(INSN, Q, T1, T2, W, N, V)	\
  TEST_VSXI_N1(INSN, Q, T1, T2, W, N, V)

  DECL_VARIABLE_ALL_VARIANTS(vector);
  DECL_VARIABLE_ALL_VARIANTS(vector2);
  DECL_VARIABLE_ALL_VARIANTS(vector_res);

  clean_results ();

  /* Initialize input "vector" from "buffer".  */
  TEST_MACRO_ALL_VARIANTS_2_5(VLOAD, vector, buffer);

  /* Fill input vector2 with arbitrary values.  */
  VDUP(vector2, , int, s, 8, 8, 2);
  VDUP(vector2, , int, s, 16, 4, -4);
  VDUP(vector2, , int, s, 32, 2, 3);
  VDUP(vector2, , int, s, 64, 1, 100);
  VDUP(vector2, , uint, u, 8, 8, 20);
  VDUP(vector2, , uint, u, 16, 4, 30);
  VDUP(vector2, , uint, u, 32, 2, 40);
  VDUP(vector2, , uint, u, 64, 1, 2);
  VDUP(vector2, , poly, p, 8, 8, 20);
  VDUP(vector2, , poly, p, 16, 4, 30);
  VDUP(vector2, q, int, s, 8, 16, -10);
  VDUP(vector2, q, int, s, 16, 8, -20);
  VDUP(vector2, q, int, s, 32, 4, -30);
  VDUP(vector2, q, int, s, 64, 2, 24);
  VDUP(vector2, q, uint, u, 8, 16, 12);
  VDUP(vector2, q, uint, u, 16, 8, 3);
  VDUP(vector2, q, uint, u, 32, 4, 55);
  VDUP(vector2, q, uint, u, 64, 2, 3);
  VDUP(vector2, q, poly, p, 8, 16, 12);
  VDUP(vector2, q, poly, p, 16, 8, 3);

  /* Choose shift amount arbitrarily.  */
  TEST_VSXI_N(INSN_NAME, , int, s, 8, 8, 4);
  TEST_VSXI_N(INSN_NAME, , int, s, 16, 4, 3);
  TEST_VSXI_N(INSN_NAME, , int, s, 32, 2, 1);
  TEST_VSXI_N(INSN_NAME, , int, s, 64, 1, 32);
  TEST_VSXI_N(INSN_NAME, , uint, u, 8, 8, 2);
  TEST_VSXI_N(INSN_NAME, , uint, u, 16, 4, 10);
  TEST_VSXI_N(INSN_NAME, , uint, u, 32, 2, 30);
  TEST_VSXI_N(INSN_NAME, , uint, u, 64, 1, 3);
  TEST_VSXI_N(INSN_NAME, , poly, p, 8, 8, 2);
  TEST_VSXI_N(INSN_NAME, , poly, p, 16, 4, 10);
  TEST_VSXI_N(INSN_NAME, q, int, s, 8, 16, 5);
  TEST_VSXI_N(INSN_NAME, q, int, s, 16, 8, 3);
  TEST_VSXI_N(INSN_NAME, q, int, s, 32, 4, 20);
  TEST_VSXI_N(INSN_NAME, q, int, s, 64, 2, 16);
  TEST_VSXI_N(INSN_NAME, q, uint, u, 8, 16, 3);
  TEST_VSXI_N(INSN_NAME, q, uint, u, 16, 8, 12);
  TEST_VSXI_N(INSN_NAME, q, uint, u, 32, 4, 23);
  TEST_VSXI_N(INSN_NAME, q, uint, u, 64, 2, 53);
  TEST_VSXI_N(INSN_NAME, q, poly, p, 8, 16, 3);
  TEST_VSXI_N(INSN_NAME, q, poly, p, 16, 8, 12);

  CHECK(TEST_MSG, int, 8, 8, PRIx8, expected, "");
  CHECK(TEST_MSG, int, 16, 4, PRIx16, expected, "");
  CHECK(TEST_MSG, int, 32, 2, PRIx32, expected, "");
  CHECK(TEST_MSG, int, 64, 1, PRIx64, expected, "");
  CHECK(TEST_MSG, uint, 8, 8, PRIx8, expected, "");
  CHECK(TEST_MSG, uint, 16, 4, PRIx16, expected, "");
  CHECK(TEST_MSG, uint, 32, 2, PRIx32, expected, "");
  CHECK(TEST_MSG, uint, 64, 1, PRIx64, expected, "");
  CHECK_POLY(TEST_MSG, poly, 8, 8, PRIx8, expected, "");
  CHECK_POLY(TEST_MSG, poly, 16, 4, PRIx16, expected, "");
  CHECK(TEST_MSG, int, 8, 16, PRIx8, expected, "");
  CHECK(TEST_MSG, int, 16, 8, PRIx16, expected, "");
  CHECK(TEST_MSG, int, 32, 4, PRIx32, expected, "");
  CHECK(TEST_MSG, uint, 8, 16, PRIx8, expected, "");
  CHECK(TEST_MSG, uint, 16, 8, PRIx16, expected, "");
  CHECK(TEST_MSG, uint, 32, 4, PRIx32, expected, "");
  CHECK_POLY(TEST_MSG, poly, 8, 16, PRIx8, expected, "");
  CHECK_POLY(TEST_MSG, poly, 16, 8, PRIx16, expected, "");

#ifdef EXTRA_TESTS
  EXTRA_TESTS();
#endif
}

int main (void)
{
  FNNAME (INSN_NAME) ();
  return 0;
}
