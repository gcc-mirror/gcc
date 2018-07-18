#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

/* Expected results.  */
VECT_VAR_DECL(expected,int,8,8) [] = { 0xf0, 0xf1, 0xf2, 0xf3,
				       0xf4, 0xf5, 0xf6, 0xf7 };
VECT_VAR_DECL(expected,int,16,4) [] = { 0xfff0, 0xfff1, 0xfff2, 0xfff3 };
VECT_VAR_DECL(expected,int,32,2) [] = { 0xfffffff0, 0xfffffff1 };
VECT_VAR_DECL(expected,uint,8,8) [] = { 0xf0, 0xf1, 0xf2, 0xf3,
					0xf4, 0xf5, 0xf6, 0xf7 };
VECT_VAR_DECL(expected,uint,16,4) [] = { 0xfff0, 0xfff1, 0xfff2, 0xfff3 };
VECT_VAR_DECL(expected,uint,32,2) [] = { 0xfffffff0, 0xfffffff1 };

#define TEST_MSG "VMOVN"
void exec_vmovn (void)
{
  /* Basic test: vec64=vmovn(vec128), then store the result.  */
#define TEST_VMOVN(T1, T2, W, W2, N)					\
  VECT_VAR(vector64, T1, W2, N) =					\
    vmovn_##T2##W(VECT_VAR(vector128, T1, W, N));			\
  vst1_##T2##W2(VECT_VAR(result, T1, W2, N), VECT_VAR(vector64, T1, W2, N))

  DECL_VARIABLE_64BITS_VARIANTS(vector64);
  DECL_VARIABLE_128BITS_VARIANTS(vector128);

  TEST_MACRO_128BITS_VARIANTS_2_5(VLOAD, vector128, buffer);

  clean_results ();

  TEST_VMOVN(int, s, 16, 8, 8);
  TEST_VMOVN(int, s, 32, 16, 4);
  TEST_VMOVN(int, s, 64, 32, 2);
  TEST_VMOVN(uint, u, 16, 8, 8);
  TEST_VMOVN(uint, u, 32, 16, 4);
  TEST_VMOVN(uint, u, 64, 32, 2);

  CHECK(TEST_MSG, int, 8, 8, PRIx8, expected, "");
  CHECK(TEST_MSG, int, 16, 4, PRIx16, expected, "");
  CHECK(TEST_MSG, int, 32, 2, PRIx32, expected, "");
  CHECK(TEST_MSG, uint, 8, 8, PRIx8, expected, "");
  CHECK(TEST_MSG, uint, 16, 4, PRIx16, expected, "");
  CHECK(TEST_MSG, uint, 32, 2, PRIx32, expected, "");
}

int main (void)
{
  exec_vmovn ();
  return 0;
}
