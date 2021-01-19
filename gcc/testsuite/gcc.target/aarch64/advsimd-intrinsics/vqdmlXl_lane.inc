#define FNNAME1(NAME) exec_ ## NAME
#define FNNAME(NAME) FNNAME1(NAME)

void FNNAME (INSN_NAME) (void)
{
  /* vector_res = vqdmlXl_lane(vector, vector3, vector4, lane),
     then store the result.  */
#define TEST_VQDMLXL_LANE1(INSN, T1, T2, W, W2, N, V, CMT) \
  Set_Neon_Cumulative_Sat(0, VECT_VAR(vector_res, T1, W, N));		\
  VECT_VAR(vector_res, T1, W, N) =					\
    INSN##_##T2##W2(VECT_VAR(vector, T1, W, N),				\
		    VECT_VAR(vector3, T1, W2, N),			\
		    VECT_VAR(vector4, T1, W2, N),			\
		    V);							\
  vst1q_##T2##W(VECT_VAR(result, T1, W, N),				\
		VECT_VAR(vector_res, T1, W, N))

#define TEST_VQDMLXL_LANE(INSN, T1, T2, W, W2, N, V, CMT) \
  TEST_VQDMLXL_LANE1(INSN, T1, T2, W, W2, N, V, CMT)

  DECL_VARIABLE(vector, int, 32, 4);
  DECL_VARIABLE(vector3, int, 16, 4);
  DECL_VARIABLE(vector4, int, 16, 4);
  DECL_VARIABLE(vector_res, int, 32, 4);

  DECL_VARIABLE(vector, int, 64, 2);
  DECL_VARIABLE(vector3, int, 32, 2);
  DECL_VARIABLE(vector4, int, 32, 2);
  DECL_VARIABLE(vector_res, int, 64, 2);

  clean_results ();

  VLOAD(vector, buffer, q, int, s, 32, 4);
  VLOAD(vector, buffer, q, int, s, 64, 2);

  VDUP(vector3, , int, s, 16, 4, 0x55);
  VDUP(vector4, , int, s, 16, 4, 0xBB);
  VDUP(vector3, , int, s, 32, 2, 0x55);
  VDUP(vector4, , int, s, 32, 2, 0xBB);

  TEST_VQDMLXL_LANE(INSN_NAME, int, s, 32, 16, 4, 0, "");
  TEST_VQDMLXL_LANE(INSN_NAME, int, s, 64, 32, 2, 0, "");

  CHECK(TEST_MSG, int, 32, 4, PRIx32, expected, "");
  CHECK(TEST_MSG, int, 64, 2, PRIx64, expected, "");

#define TEST_MSG2 "(mul with input=0)"
  VDUP(vector3, , int, s, 16, 4, 0);
  VDUP(vector3, , int, s, 32, 2, 0);
  TEST_VQDMLXL_LANE(INSN_NAME, int, s, 32, 16, 4, 0, TEST_MSG2);
  TEST_VQDMLXL_LANE(INSN_NAME, int, s, 64, 32, 2, 0, TEST_MSG2);

  CHECK(TEST_MSG, int, 32, 4, PRIx32, expected2, TEST_MSG2);
  CHECK(TEST_MSG, int, 64, 2, PRIx64, expected2, TEST_MSG2);

#define TEST_MSG3 "(mul with saturation)"
  VDUP(vector3, , int, s, 16, 4, 0x8000);
  VDUP(vector3, , int, s, 32, 2, 0x80000000);
  VDUP(vector4, , int, s, 16, 4, 0x8000);
  VDUP(vector4, , int, s, 32, 2, 0x80000000);
  TEST_VQDMLXL_LANE(INSN_NAME, int, s, 32, 16, 4, 0, TEST_MSG3);
  TEST_VQDMLXL_LANE(INSN_NAME, int, s, 64, 32, 2, 0, TEST_MSG3);

  CHECK(TEST_MSG, int, 32, 4, PRIx32, expected3, TEST_MSG3);
  CHECK(TEST_MSG, int, 64, 2, PRIx64, expected3, TEST_MSG3);
}

int main (void)
{
  FNNAME (INSN_NAME) ();
  return 0;
}
