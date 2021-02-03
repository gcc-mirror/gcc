#define FNNAME1(NAME) exec_ ## NAME
#define FNNAME(NAME) FNNAME1(NAME)

void FNNAME (INSN_NAME) (void)
{
  /* vector_res = OP(vector, vector3, vector4),
     then store the result.  */
#define TEST_VMLXL_HIGH1(INSN, T1, T2, W1, W2, N1, N2)			       \
  VECT_VAR(vector_res, T1, W1, N1) =					       \
    INSN##_##T2##W2(VECT_VAR(vector, T1, W1, N1),			       \
                    VECT_VAR(vector3, T1, W2, N2),			       \
                    VECT_VAR(vector4, T1, W2, N2));			       \
  vst1q_##T2##W1(VECT_VAR(result, T1, W1, N1), VECT_VAR(vector_res, T1, W1, N1))

#define TEST_VMLXL_HIGH(INSN, T1, T2, W1, W2, N1, N2)			       \
  TEST_VMLXL_HIGH1(INSN, T1, T2, W1, W2, N1, N2)

  DECL_VARIABLE(vector, int, 16, 8);
  DECL_VARIABLE(vector3, int, 8, 16);
  DECL_VARIABLE(vector4, int, 8, 16);
  DECL_VARIABLE(vector_res, int, 16, 8);

  DECL_VARIABLE(vector, int, 32, 4);
  DECL_VARIABLE(vector3, int, 16, 8);
  DECL_VARIABLE(vector4, int, 16, 8);
  DECL_VARIABLE(vector_res, int, 32, 4);

  DECL_VARIABLE(vector, int, 64, 2);
  DECL_VARIABLE(vector3, int, 32, 4);
  DECL_VARIABLE(vector4, int, 32, 4);
  DECL_VARIABLE(vector_res, int, 64, 2);

  DECL_VARIABLE(vector, uint, 16, 8);
  DECL_VARIABLE(vector3, uint, 8, 16);
  DECL_VARIABLE(vector4, uint, 8, 16);
  DECL_VARIABLE(vector_res, uint, 16, 8);

  DECL_VARIABLE(vector, uint, 32, 4);
  DECL_VARIABLE(vector3, uint, 16, 8);
  DECL_VARIABLE(vector4, uint, 16, 8);
  DECL_VARIABLE(vector_res, uint, 32, 4);

  DECL_VARIABLE(vector, uint, 64, 2);
  DECL_VARIABLE(vector3, uint, 32, 4);
  DECL_VARIABLE(vector4, uint, 32, 4);
  DECL_VARIABLE(vector_res, uint, 64, 2);

  clean_results ();

  VLOAD(vector, buffer, q, int, s, 16, 8);
  VLOAD(vector, buffer, q, int, s, 32, 4);
  VLOAD(vector, buffer, q, int, s, 64, 2);
  VLOAD(vector, buffer, q, uint, u, 16, 8);
  VLOAD(vector, buffer, q, uint, u, 32, 4);
  VLOAD(vector, buffer, q, uint, u, 64, 2);

  VDUP(vector3, q, int, s, 8, 16, 0x55);
  VDUP(vector4, q, int, s, 8, 16, 0xBB);
  VDUP(vector3, q, int, s, 16, 8, 0x55);
  VDUP(vector4, q, int, s, 16, 8, 0xBB);
  VDUP(vector3, q, int, s, 32, 4, 0x55);
  VDUP(vector4, q, int, s, 32, 4, 0xBB);
  VDUP(vector3, q, uint, u, 8, 16, 0x55);
  VDUP(vector4, q, uint, u, 8, 16, 0xBB);
  VDUP(vector3, q, uint, u, 16, 8, 0x55);
  VDUP(vector4, q, uint, u, 16, 8, 0xBB);
  VDUP(vector3, q, uint, u, 32, 4, 0x55);
  VDUP(vector4, q, uint, u, 32, 4, 0xBB);

  TEST_VMLXL_HIGH(INSN_NAME, int, s, 16, 8, 8, 16);
  TEST_VMLXL_HIGH(INSN_NAME, int, s, 32, 16, 4, 8);
  TEST_VMLXL_HIGH(INSN_NAME, int, s, 64, 32, 2, 4);
  TEST_VMLXL_HIGH(INSN_NAME, uint, u, 16, 8, 8, 16);
  TEST_VMLXL_HIGH(INSN_NAME, uint, u, 32, 16, 4, 8);
  TEST_VMLXL_HIGH(INSN_NAME, uint, u, 64, 32, 2, 4);

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
