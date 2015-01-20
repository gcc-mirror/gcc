#define FNNAME1(NAME) exec_ ## NAME
#define FNNAME(NAME) FNNAME1(NAME)

void FNNAME (INSN_NAME) (void)
{
#define DECL_VMLX_LANE(VAR)			\
  DECL_VARIABLE(VAR, int, 16, 4);		\
  DECL_VARIABLE(VAR, int, 32, 2);		\
  DECL_VARIABLE(VAR, uint, 16, 4);		\
  DECL_VARIABLE(VAR, uint, 32, 2);		\
  DECL_VARIABLE(VAR, float, 32, 2);		\
  DECL_VARIABLE(VAR, int, 16, 8);		\
  DECL_VARIABLE(VAR, int, 32, 4);		\
  DECL_VARIABLE(VAR, uint, 16, 8);		\
  DECL_VARIABLE(VAR, uint, 32, 4);		\
  DECL_VARIABLE(VAR, float, 32, 4)

  /* vector_res = vmlx_lane(vector, vector2, vector3, lane),
     then store the result.  */
#define TEST_VMLX_LANE1(INSN, Q, T1, T2, W, N, N2, L)			\
  VECT_VAR(vector_res, T1, W, N) =					\
    INSN##Q##_lane_##T2##W(VECT_VAR(vector, T1, W, N),			\
			   VECT_VAR(vector2, T1, W, N),			\
			   VECT_VAR(vector3, T1, W, N2),		\
			   L);						\
  vst1##Q##_##T2##W(VECT_VAR(result, T1, W, N), \
		    VECT_VAR(vector_res, T1, W, N))

#define TEST_VMLX_LANE(INSN, Q, T1, T2, W, N, N2, V)	\
  TEST_VMLX_LANE1(INSN, Q, T1, T2, W, N, N2, V)

  DECL_VMLX_LANE(vector);
  DECL_VMLX_LANE(vector2);
  DECL_VMLX_LANE(vector_res);

  DECL_VARIABLE(vector3, int, 16, 4);
  DECL_VARIABLE(vector3, int, 32, 2);
  DECL_VARIABLE(vector3, uint, 16, 4);
  DECL_VARIABLE(vector3, uint, 32, 2);
  DECL_VARIABLE(vector3, float, 32, 2);

  clean_results ();

  VLOAD(vector, buffer, , int, s, 16, 4);
  VLOAD(vector, buffer, , int, s, 32, 2);
  VLOAD(vector, buffer, , uint, u, 16, 4);
  VLOAD(vector, buffer, , uint, u, 32, 2);
  VLOAD(vector, buffer, q, int, s, 16, 8);
  VLOAD(vector, buffer, q, int, s, 32, 4);
  VLOAD(vector, buffer, q, uint, u, 16, 8);
  VLOAD(vector, buffer, q, uint, u, 32, 4);
  VLOAD(vector, buffer, , float, f, 32, 2);
  VLOAD(vector, buffer, q, float, f, 32, 4);

  VDUP(vector2, , int, s, 16, 4, 0x55);
  VDUP(vector2, , int, s, 32, 2, 0x55);
  VDUP(vector2, , uint, u, 16, 4, 0x55);
  VDUP(vector2, , uint, u, 32, 2, 0x55);
  VDUP(vector2, , float, f, 32, 2, 55.3f);
  VDUP(vector2, q, int, s, 16, 8, 0x55);
  VDUP(vector2, q, int, s, 32, 4, 0x55);
  VDUP(vector2, q, uint, u, 16, 8, 0x55);
  VDUP(vector2, q, uint, u, 32, 4, 0x55);
  VDUP(vector2, q, float, f, 32, 4, 55.8f);

  VDUP(vector3, , int, s, 16, 4, 0xBB);
  VDUP(vector3, , int, s, 32, 2, 0xBB);
  VDUP(vector3, , uint, u, 16, 4, 0xBB);
  VDUP(vector3, , uint, u, 32, 2, 0xBB);
  VDUP(vector3, , float, f, 32, 2, 11.34f);

  /* Choose lane arbitrarily.  */
  TEST_VMLX_LANE(INSN_NAME, , int, s, 16, 4, 4, 2);
  TEST_VMLX_LANE(INSN_NAME, , int, s, 32, 2, 2, 1);
  TEST_VMLX_LANE(INSN_NAME, , uint, u, 16, 4, 4, 2);
  TEST_VMLX_LANE(INSN_NAME, , uint, u, 32, 2, 2, 1);
  TEST_VMLX_LANE(INSN_NAME, , float, f, 32, 2, 2, 1);
  TEST_VMLX_LANE(INSN_NAME, q, int, s, 16, 8, 4, 3);
  TEST_VMLX_LANE(INSN_NAME, q, int, s, 32, 4, 2, 1);
  TEST_VMLX_LANE(INSN_NAME, q, uint, u, 16, 8, 4, 2);
  TEST_VMLX_LANE(INSN_NAME, q, uint, u, 32, 4, 2, 1);
  TEST_VMLX_LANE(INSN_NAME, q, float, f, 32, 4, 2, 1);

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
