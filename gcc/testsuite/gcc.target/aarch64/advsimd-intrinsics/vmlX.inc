#define FNNAME1(NAME) exec_ ## NAME
#define FNNAME(NAME) FNNAME1(NAME)

void FNNAME (INSN_NAME) (void)
{
#define DECL_VMLX(T, W, N)			\
  DECL_VARIABLE(vector1, T, W, N);		\
  DECL_VARIABLE(vector2, T, W, N);		\
  DECL_VARIABLE(vector3, T, W, N);		\
  DECL_VARIABLE(vector_res, T, W, N)

  /* vector_res = vmla(vector, vector3, vector4),
     then store the result.  */
#define TEST_VMLX1(INSN, Q, T1, T2, W, N)				\
  VECT_VAR(vector_res, T1, W, N) =                                      \
    INSN##Q##_##T2##W(VECT_VAR(vector1, T1, W, N),			\
		      VECT_VAR(vector2, T1, W, N),			\
		      VECT_VAR(vector3, T1, W, N));			\
  vst1##Q##_##T2##W(VECT_VAR(result, T1, W, N),                         \
		    VECT_VAR(vector_res, T1, W, N))

#define TEST_VMLX(INSN, Q, T1, T2, W, N)	\
  TEST_VMLX1(INSN, Q, T1, T2, W, N)

  DECL_VMLX(int, 8, 8);
  DECL_VMLX(int, 16, 4);
  DECL_VMLX(int, 32, 2);
  DECL_VMLX(uint, 8, 8);
  DECL_VMLX(uint, 16, 4);
  DECL_VMLX(uint, 32, 2);
  DECL_VMLX(float, 32, 2);
  DECL_VMLX(int, 8, 16);
  DECL_VMLX(int, 16, 8);
  DECL_VMLX(int, 32, 4);
  DECL_VMLX(uint, 8, 16);
  DECL_VMLX(uint, 16, 8);
  DECL_VMLX(uint, 32, 4);
  DECL_VMLX(float, 32, 4);

  clean_results ();

  VLOAD(vector1, buffer, , int, s, 8, 8);
  VLOAD(vector1, buffer, , int, s, 16, 4);
  VLOAD(vector1, buffer, , int, s, 32, 2);
  VLOAD(vector1, buffer, , uint, u, 8, 8);
  VLOAD(vector1, buffer, , uint, u, 16, 4);
  VLOAD(vector1, buffer, , uint, u, 32, 2);
  VLOAD(vector1, buffer, , float, f, 32, 2);
  VLOAD(vector1, buffer, q, int, s, 8, 16);
  VLOAD(vector1, buffer, q, int, s, 16, 8);
  VLOAD(vector1, buffer, q, int, s, 32, 4);
  VLOAD(vector1, buffer, q, uint, u, 8, 16);
  VLOAD(vector1, buffer, q, uint, u, 16, 8);
  VLOAD(vector1, buffer, q, uint, u, 32, 4);
  VLOAD(vector1, buffer, q, float, f, 32, 4);

  VDUP(vector2, , int, s, 8, 8, 0x11);
  VDUP(vector2, , int, s, 16, 4, 0x22);
  VDUP(vector2, , int, s, 32, 2, 0x33);
  VDUP(vector2, , uint, u, 8, 8, 0x44);
  VDUP(vector2, , uint, u, 16, 4, 0x55);
  VDUP(vector2, , uint, u, 32, 2, 0x66);
  VDUP(vector2, , float, f, 32, 2, 33.1f);
  VDUP(vector2, q, int, s, 8, 16, 0x77);
  VDUP(vector2, q, int, s, 16, 8, 0x88);
  VDUP(vector2, q, int, s, 32, 4, 0x99);
  VDUP(vector2, q, uint, u, 8, 16, 0xAA);
  VDUP(vector2, q, uint, u, 16, 8, 0xBB);
  VDUP(vector2, q, uint, u, 32, 4, 0xCC);
  VDUP(vector2, q, float, f, 32, 4, 99.2f);

  VDUP(vector3, , int, s, 8, 8, 0xFF);
  VDUP(vector3, , int, s, 16, 4, 0xEE);
  VDUP(vector3, , int, s, 32, 2, 0xDD);
  VDUP(vector3, , uint, u, 8, 8, 0xCC);
  VDUP(vector3, , uint, u, 16, 4, 0xBB);
  VDUP(vector3, , uint, u, 32, 2, 0xAA);
  VDUP(vector3, , float, f, 32, 2, 10.23f);
  VDUP(vector3, q, int, s, 8, 16, 0x99);
  VDUP(vector3, q, int, s, 16, 8, 0x88);
  VDUP(vector3, q, int, s, 32, 4, 0x77);
  VDUP(vector3, q, uint, u, 8, 16, 0x66);
  VDUP(vector3, q, uint, u, 16, 8, 0x55);
  VDUP(vector3, q, uint, u, 32, 4, 0x44);
  VDUP(vector3, q, float, f, 32, 4, 77.8f);

  TEST_VMLX(INSN_NAME, , int, s, 8, 8);
  TEST_VMLX(INSN_NAME, , int, s, 16, 4);
  TEST_VMLX(INSN_NAME, , int, s, 32, 2);
  TEST_VMLX(INSN_NAME, , uint, u, 8, 8);
  TEST_VMLX(INSN_NAME, , uint, u, 16, 4);
  TEST_VMLX(INSN_NAME, , uint, u, 32, 2);
  TEST_VMLX(INSN_NAME, , float, f, 32, 2);
  TEST_VMLX(INSN_NAME, q, int, s, 8, 16);
  TEST_VMLX(INSN_NAME, q, int, s, 16, 8);
  TEST_VMLX(INSN_NAME, q, int, s, 32, 4);
  TEST_VMLX(INSN_NAME, q, uint, u, 8, 16);
  TEST_VMLX(INSN_NAME, q, uint, u, 16, 8);
  TEST_VMLX(INSN_NAME, q, uint, u, 32, 4);
  TEST_VMLX(INSN_NAME, q, float, f, 32, 4);

  CHECK(TEST_MSG, int, 8, 8, PRIx8, expected, "");
  CHECK(TEST_MSG, int, 16, 4, PRIx16, expected, "");
  CHECK(TEST_MSG, int, 32, 2, PRIx32, expected, "");
  CHECK(TEST_MSG, uint, 8, 8, PRIx8, expected, "");
  CHECK(TEST_MSG, uint, 16, 4, PRIx16, expected, "");
  CHECK(TEST_MSG, uint, 32, 2, PRIx32, expected, "");
  CHECK_FP(TEST_MSG, float, 32, 2, PRIx32, expected, "");
  CHECK(TEST_MSG, int, 8, 16, PRIx8, expected, "");
  CHECK(TEST_MSG, int, 16, 8, PRIx16, expected, "");
  CHECK(TEST_MSG, int, 32, 4, PRIx32, expected, "");
  CHECK(TEST_MSG, uint, 8, 16, PRIx8, expected, "");
  CHECK(TEST_MSG, uint, 16, 8, PRIx16, expected, "");
  CHECK(TEST_MSG, uint, 32, 4, PRIx32, expected, "");
  CHECK_FP(TEST_MSG, float, 32, 4, PRIx32, expected, "");
}

int main (void)
{
  FNNAME (INSN_NAME) ();
  return 0;
}

