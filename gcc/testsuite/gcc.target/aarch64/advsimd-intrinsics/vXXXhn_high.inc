#define FNNAME1(NAME) exec_ ## NAME
#define FNNAME(NAME) FNNAME1(NAME)

void FNNAME (INSN_NAME) (void)
{
  /* Basic test: v128_r=vXXXhn_high(v64_r, v128_a, v128_b), store result.  */
#define TEST_VXXXHN_HIGH1(INSN, T1, T2, W1, W2, N1, N2)				\
  VECT_VAR(v128_r, T1, W2, N2) = INSN##_##T2##W1(VECT_VAR(v64_r, T1, W2, N1),	\
						 VECT_VAR(v128_a, T1, W1, N1),	\
						 VECT_VAR(v128_b, T1, W1, N1));	\
  vst1q_##T2##W2(VECT_VAR(result, T1, W2, N2), VECT_VAR(v128_r, T1, W2, N2))

#define TEST_VXXXHN_HIGH(INSN, T1, T2, W1, W2, N1, N2)				\
  TEST_VXXXHN_HIGH1(INSN, T1, T2, W1, W2, N1, N2)

  DECL_VARIABLE_128BITS_VARIANTS(v128_r);
  DECL_VARIABLE_64BITS_VARIANTS(v64_r);
  DECL_VARIABLE_128BITS_VARIANTS(v128_a);
  DECL_VARIABLE_128BITS_VARIANTS(v128_b);

  clean_results ();

  /* Fill v64_r with a value easy to recognise in the result vector. */
  VDUP(v64_r, , int, s, 8, 8, 0x5);
  VDUP(v64_r, , int, s, 16, 4, 0x5);
  VDUP(v64_r, , int, s, 32, 2, 0x5);
  VDUP(v64_r, , uint, u, 8, 8, 0x5);
  VDUP(v64_r, , uint, u, 16, 4, 0x5);
  VDUP(v64_r, , uint, u, 32, 2, 0x5);

  /* Fill input v128_a and v128_b with arbitrary values. */
  VDUP(v128_a, q, int, s, 16, 8, 50*(UINT8_MAX+1));
  VDUP(v128_a, q, int, s, 32, 4, 50*(UINT16_MAX+1));
  VDUP(v128_a, q, int, s, 64, 2, 24*((uint64_t)UINT32_MAX+1));
  VDUP(v128_a, q, uint, u, 16, 8, 3*(UINT8_MAX+1));
  VDUP(v128_a, q, uint, u, 32, 4, 55*(UINT16_MAX+1));
  VDUP(v128_a, q, uint, u, 64, 2, 3*((uint64_t)UINT32_MAX+1));

  VDUP(v128_b, q, int, s, 16, 8, (uint16_t)UINT8_MAX);
  VDUP(v128_b, q, int, s, 32, 4, (uint32_t)UINT16_MAX);
  VDUP(v128_b, q, int, s, 64, 2, (uint64_t)UINT32_MAX);
  VDUP(v128_b, q, uint, u, 16, 8, (uint16_t)UINT8_MAX);
  VDUP(v128_b, q, uint, u, 32, 4, (uint32_t)UINT16_MAX);
  VDUP(v128_b, q, uint, u, 64, 2, (uint64_t)UINT32_MAX);

  TEST_VXXXHN_HIGH(INSN_NAME, int, s, 16, 8, 8, 16);
  TEST_VXXXHN_HIGH(INSN_NAME, int, s, 32, 16, 4, 8);
  TEST_VXXXHN_HIGH(INSN_NAME, int, s, 64, 32, 2, 4);
  TEST_VXXXHN_HIGH(INSN_NAME, uint, u, 16, 8, 8, 16);
  TEST_VXXXHN_HIGH(INSN_NAME, uint, u, 32, 16, 4, 8);
  TEST_VXXXHN_HIGH(INSN_NAME, uint, u, 64, 32, 2, 4);

  CHECK(TEST_MSG, int, 8, 16, PRIx8, expected, "");
  CHECK(TEST_MSG, int, 16, 8, PRIx16, expected, "");
  CHECK(TEST_MSG, int, 32, 4, PRIx32, expected, "");
  CHECK(TEST_MSG, uint, 8, 16, PRIx8, expected, "");
  CHECK(TEST_MSG, uint, 16, 8, PRIx16, expected, "");
  CHECK(TEST_MSG, uint, 32, 4, PRIx32, expected, "");
}

int main (void)
{
  FNNAME (INSN_NAME) ();
  return 0;
}
