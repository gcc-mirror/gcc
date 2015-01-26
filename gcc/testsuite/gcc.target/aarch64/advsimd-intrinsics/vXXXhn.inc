#define FNNAME1(NAME) exec_ ## NAME
#define FNNAME(NAME) FNNAME1(NAME)

void FNNAME (INSN_NAME) (void)
{
  /* Basic test: vec64=vXXXhn(vec128_a, vec128_b), then store the result.  */
#define TEST_VXXXHN1(INSN, T1, T2, W, W2, N)				\
  VECT_VAR(vector64, T1, W2, N) = INSN##_##T2##W(VECT_VAR(vector1, T1, W, N), \
						 VECT_VAR(vector2, T1, W, N)); \
  vst1_##T2##W2(VECT_VAR(result, T1, W2, N), VECT_VAR(vector64, T1, W2, N))

#define TEST_VXXXHN(INSN, T1, T2, W, W2, N)	\
  TEST_VXXXHN1(INSN, T1, T2, W, W2, N)

  DECL_VARIABLE_64BITS_VARIANTS(vector64);
  DECL_VARIABLE_128BITS_VARIANTS(vector1);
  DECL_VARIABLE_128BITS_VARIANTS(vector2);

  clean_results ();

  /* Fill input vector1 and vector2 with arbitrary values */
  VDUP(vector1, q, int, s, 16, 8, 50*(UINT8_MAX+1));
  VDUP(vector1, q, int, s, 32, 4, 50*(UINT16_MAX+1));
  VDUP(vector1, q, int, s, 64, 2, 24*((uint64_t)UINT32_MAX+1));
  VDUP(vector1, q, uint, u, 16, 8, 3*(UINT8_MAX+1));
  VDUP(vector1, q, uint, u, 32, 4, 55*(UINT16_MAX+1));
  VDUP(vector1, q, uint, u, 64, 2, 3*((uint64_t)UINT32_MAX+1));

  VDUP(vector2, q, int, s, 16, 8, (uint16_t)UINT8_MAX);
  VDUP(vector2, q, int, s, 32, 4, (uint32_t)UINT16_MAX);
  VDUP(vector2, q, int, s, 64, 2, (uint64_t)UINT32_MAX);
  VDUP(vector2, q, uint, u, 16, 8, (uint16_t)UINT8_MAX);
  VDUP(vector2, q, uint, u, 32, 4, (uint32_t)UINT16_MAX);
  VDUP(vector2, q, uint, u, 64, 2, (uint64_t)UINT32_MAX);

  TEST_VXXXHN(INSN_NAME, int, s, 16, 8, 8);
  TEST_VXXXHN(INSN_NAME, int, s, 32, 16, 4);
  TEST_VXXXHN(INSN_NAME, int, s, 64, 32, 2);
  TEST_VXXXHN(INSN_NAME, uint, u, 16, 8, 8);
  TEST_VXXXHN(INSN_NAME, uint, u, 32, 16, 4);
  TEST_VXXXHN(INSN_NAME, uint, u, 64, 32, 2);

  CHECK(TEST_MSG, int, 8, 8, PRIx8, expected, "");
  CHECK(TEST_MSG, int, 16, 4, PRIx16, expected, "");
  CHECK(TEST_MSG, int, 32, 2, PRIx32, expected, "");
  CHECK(TEST_MSG, uint, 8, 8, PRIx8, expected, "");
  CHECK(TEST_MSG, uint, 16, 4, PRIx16, expected, "");
  CHECK(TEST_MSG, uint, 32, 2, PRIx32, expected, "");
}

int main (void)
{
  FNNAME (INSN_NAME) ();
  return 0;
}
