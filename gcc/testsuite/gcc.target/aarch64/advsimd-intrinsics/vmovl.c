#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

/* Expected results.  */
VECT_VAR_DECL(expected,int,16,8) [] = { 0xfff0, 0xfff1, 0xfff2, 0xfff3,
					0xfff4, 0xfff5, 0xfff6, 0xfff7 };
VECT_VAR_DECL(expected,int,32,4) [] = { 0xfffffff0, 0xfffffff1,
					0xfffffff2, 0xfffffff3 };
VECT_VAR_DECL(expected,int,64,2) [] = { 0xfffffffffffffff0,
					0xfffffffffffffff1 };
VECT_VAR_DECL(expected,uint,16,8) [] = { 0xf0, 0xf1, 0xf2, 0xf3,
					 0xf4, 0xf5, 0xf6, 0xf7 };
VECT_VAR_DECL(expected,uint,32,4) [] = { 0xfff0, 0xfff1, 0xfff2, 0xfff3 };
VECT_VAR_DECL(expected,uint,64,2) [] = { 0xfffffff0, 0xfffffff1 };

#define TEST_MSG "VMOVL"
void exec_vmovl (void)
{
  /* Basic test: vec128=vmovl(vec64), then store the result.  */
#define TEST_VMOVL(T1, T2, W, W2, N)					\
  VECT_VAR(vector128, T1, W2, N) =					\
    vmovl_##T2##W(VECT_VAR(vector64, T1, W, N));			\
  vst1q_##T2##W2(VECT_VAR(result, T1, W2, N), VECT_VAR(vector128, T1, W2, N))

  DECL_VARIABLE_64BITS_VARIANTS(vector64);
  DECL_VARIABLE_128BITS_VARIANTS(vector128);

  TEST_MACRO_64BITS_VARIANTS_2_5(VLOAD, vector64, buffer);

  clean_results ();

  TEST_VMOVL(int, s, 8, 16, 8);
  TEST_VMOVL(int, s, 16, 32, 4);
  TEST_VMOVL(int, s, 32, 64, 2);
  TEST_VMOVL(uint, u, 8, 16, 8);
  TEST_VMOVL(uint, u, 16, 32, 4);
  TEST_VMOVL(uint, u, 32, 64, 2);

  CHECK(TEST_MSG, int, 16, 8, PRIx16, expected, "");
  CHECK(TEST_MSG, int, 32, 4, PRIx32, expected, "");
  CHECK(TEST_MSG, int, 64, 2, PRIx64, expected, "");
  CHECK(TEST_MSG, uint, 16, 8, PRIx16, expected, "");
  CHECK(TEST_MSG, uint, 32, 4, PRIx32, expected, "");
  CHECK(TEST_MSG, uint, 64, 2, PRIx64, expected, "");
}

int main (void)
{
  exec_vmovl ();
  return 0;
}
