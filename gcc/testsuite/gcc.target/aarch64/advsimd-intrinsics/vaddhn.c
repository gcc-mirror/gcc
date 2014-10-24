#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

#if defined(__cplusplus)
#include <cstdint>
#else
#include <stdint.h>
#endif

/* Expected results.  */
VECT_VAR_DECL(expected,int,8,8) [] = { 0x32, 0x32, 0x32, 0x32,
				       0x32, 0x32, 0x32, 0x32 };
VECT_VAR_DECL(expected,int,16,4) [] = { 0x32, 0x32, 0x32, 0x32 };
VECT_VAR_DECL(expected,int,32,2) [] = { 0x18, 0x18 };
VECT_VAR_DECL(expected,int,64,1) [] = { 0x3333333333333333 };
VECT_VAR_DECL(expected,uint,8,8) [] = { 0x3, 0x3, 0x3, 0x3,
					0x3, 0x3, 0x3, 0x3 };
VECT_VAR_DECL(expected,uint,16,4) [] = { 0x37, 0x37, 0x37, 0x37 };
VECT_VAR_DECL(expected,uint,32,2) [] = { 0x3, 0x3 };
VECT_VAR_DECL(expected,uint,64,1) [] = { 0x3333333333333333 };
VECT_VAR_DECL(expected,poly,8,8) [] = { 0x33, 0x33, 0x33, 0x33,
					0x33, 0x33, 0x33, 0x33 };
VECT_VAR_DECL(expected,poly,16,4) [] = { 0x3333, 0x3333, 0x3333, 0x3333 };
VECT_VAR_DECL(expected,hfloat,32,2) [] = { 0x33333333, 0x33333333 };
VECT_VAR_DECL(expected,int,8,16) [] = { 0x33, 0x33, 0x33, 0x33,
					0x33, 0x33, 0x33, 0x33,
					0x33, 0x33, 0x33, 0x33,
					0x33, 0x33, 0x33, 0x33 };
VECT_VAR_DECL(expected,int,16,8) [] = {  0x3333, 0x3333, 0x3333, 0x3333,
					 0x3333, 0x3333, 0x3333, 0x3333 };
VECT_VAR_DECL(expected,int,32,4) [] = { 0x33333333, 0x33333333,
					0x33333333, 0x33333333 };
VECT_VAR_DECL(expected,int,64,2) [] = { 0x3333333333333333,
					0x3333333333333333 };
VECT_VAR_DECL(expected,uint,8,16) [] = { 0x33, 0x33, 0x33, 0x33,
					 0x33, 0x33, 0x33, 0x33,
					 0x33, 0x33, 0x33, 0x33,
					 0x33, 0x33, 0x33, 0x33 };
VECT_VAR_DECL(expected,uint,16,8) [] = { 0x3333, 0x3333, 0x3333, 0x3333,
					 0x3333, 0x3333, 0x3333, 0x3333 };
VECT_VAR_DECL(expected,uint,32,4) [] = { 0x33333333, 0x33333333,
					 0x33333333, 0x33333333 };
VECT_VAR_DECL(expected,uint,64,2) [] = { 0x3333333333333333,
					 0x3333333333333333 };
VECT_VAR_DECL(expected,poly,8,16) [] = { 0x33, 0x33, 0x33, 0x33,
					 0x33, 0x33, 0x33, 0x33,
					 0x33, 0x33, 0x33, 0x33,
					 0x33, 0x33, 0x33, 0x33 };
VECT_VAR_DECL(expected,poly,16,8) [] = { 0x3333, 0x3333, 0x3333, 0x3333,
					 0x3333, 0x3333, 0x3333, 0x3333 };
VECT_VAR_DECL(expected,hfloat,32,4) [] = { 0x33333333, 0x33333333,
					   0x33333333, 0x33333333 };

#ifndef INSN_NAME
#define INSN_NAME vaddhn
#define TEST_MSG "VADDHN"
#endif

#define FNNAME1(NAME) void exec_ ## NAME (void)
#define FNNAME(NAME) FNNAME1(NAME)

FNNAME (INSN_NAME)
{
  /* Basic test: vec64=vaddhn(vec128_a, vec128_b), then store the result.  */
#define TEST_VADDHN1(INSN, T1, T2, W, W2, N)				\
  VECT_VAR(vector64, T1, W2, N) = INSN##_##T2##W(VECT_VAR(vector1, T1, W, N), \
						 VECT_VAR(vector2, T1, W, N)); \
  vst1_##T2##W2(VECT_VAR(result, T1, W2, N), VECT_VAR(vector64, T1, W2, N))

#define TEST_VADDHN(INSN, T1, T2, W, W2, N)	\
  TEST_VADDHN1(INSN, T1, T2, W, W2, N)

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

  TEST_VADDHN(INSN_NAME, int, s, 16, 8, 8);
  TEST_VADDHN(INSN_NAME, int, s, 32, 16, 4);
  TEST_VADDHN(INSN_NAME, int, s, 64, 32, 2);
  TEST_VADDHN(INSN_NAME, uint, u, 16, 8, 8);
  TEST_VADDHN(INSN_NAME, uint, u, 32, 16, 4);
  TEST_VADDHN(INSN_NAME, uint, u, 64, 32, 2);

  CHECK_RESULTS (TEST_MSG, "");
}

int main (void)
{
  FNNAME (INSN_NAME);
  return 0;
}
