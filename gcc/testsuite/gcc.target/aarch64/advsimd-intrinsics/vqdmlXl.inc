#define FNNAME1(NAME) exec_ ## NAME
#define FNNAME(NAME) FNNAME1(NAME)

void FNNAME (INSN_NAME) (void)
{
  /* vector_res = OP(vector, vector3, vector4),
     then store the result.  */
#define TEST_VQDMLXL1(INSN, T1, T2, W, W2, N, CMT) \
  Set_Neon_Cumulative_Sat(0, VECT_VAR(vector_res, T1, W, N));		\
  VECT_VAR(vector_res, T1, W, N) =					\
    INSN##_##T2##W2(VECT_VAR(vector, T1, W, N),				\
		    VECT_VAR(vector3, T1, W2, N),			\
		    VECT_VAR(vector4, T1, W2, N));			\
    vst1q_##T2##W(VECT_VAR(result, T1, W, N),				\
		  VECT_VAR(vector_res, T1, W, N))

#define TEST_VQDMLXL(INSN, T1, T2, W, W2, N, CMT) \
  TEST_VQDMLXL1(INSN, T1, T2, W, W2, N, CMT)

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

  TEST_VQDMLXL(INSN_NAME, int, s, 32, 16, 4, "");
  TEST_VQDMLXL(INSN_NAME, int, s, 64, 32, 2, "");

  CHECK(TEST_MSG, int, 32, 4, PRIx32, expected, "");
  CHECK(TEST_MSG, int, 64, 2, PRIx64, expected, "");

  VDUP(vector3, , int, s, 16, 4, 0x8000);
  VDUP(vector4, , int, s, 16, 4, 0x8000);
  VDUP(vector3, , int, s, 32, 2, 0x80000000);
  VDUP(vector4, , int, s, 32, 2, 0x80000000);

#define TEST_MSG2 "with saturation"
  TEST_VQDMLXL(INSN_NAME, int, s, 32, 16, 4, TEST_MSG2);
  TEST_VQDMLXL(INSN_NAME, int, s, 64, 32, 2, TEST_MSG2);

  CHECK(TEST_MSG, int, 32, 4, PRIx32, expected2, TEST_MSG2);
  CHECK(TEST_MSG, int, 64, 2, PRIx64, expected2, TEST_MSG2);
}

int main (void)
{
  FNNAME (INSN_NAME) ();
  return 0;
}
