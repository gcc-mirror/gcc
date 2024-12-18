/* { dg-do compile } */
/* { dg-options "-O3 -march=armv8.2-a+sve" } */

#include <arm_sve.h>

#define PTRUE_B(BITWIDTH) svptrue_b##BITWIDTH ()

#define ROR_SVE_LSL(NAME, INPUT_TYPE, SHIFT_AMOUNT, BITWIDTH)                  \
  INPUT_TYPE                                                                   \
  NAME##_imm (INPUT_TYPE r)                                                    \
  {                                                                            \
    return svorr_u##BITWIDTH##_z (PTRUE_B (BITWIDTH),                          \
				  svlsl_n_u##BITWIDTH##_z (PTRUE_B (BITWIDTH), \
							   r, SHIFT_AMOUNT),   \
				  svlsr_n_u##BITWIDTH##_z (PTRUE_B (BITWIDTH), \
							   r, SHIFT_AMOUNT));  \
  }                                                                            \
                                                                               \
  INPUT_TYPE                                                                   \
  NAME##_operand (INPUT_TYPE r)                                                \
  {                                                                            \
    svbool_t pt = PTRUE_B (BITWIDTH);                                          \
    return svorr_u##BITWIDTH##_z (                                             \
      pt, svlsl_n_u##BITWIDTH##_z (pt, r, SHIFT_AMOUNT),                       \
      svlsr_n_u##BITWIDTH##_z (pt, r, SHIFT_AMOUNT));                          \
  }

/* Make sure that the pattern doesn't match incorrect bit-widths, eg. a shift of
   8 matching the 32-bit mode.  */

ROR_SVE_LSL (higher_ror32, svuint64_t, 64, 64);
ROR_SVE_LSL (higher_ror16, svuint32_t, 32, 32);
ROR_SVE_LSL (higher_ror8, svuint16_t, 16, 16);

ROR_SVE_LSL (lower_ror32, svuint64_t, 16, 64);
ROR_SVE_LSL (lower_ror16, svuint32_t, 8, 32);
ROR_SVE_LSL (lower_ror8, svuint16_t, 4, 16);

/* Check off-by-one cases.  */

ROR_SVE_LSL (off_1_high_ror32, svuint64_t, 33, 64);
ROR_SVE_LSL (off_1_high_ror16, svuint32_t, 17, 32);
ROR_SVE_LSL (off_1_high_ror8, svuint16_t, 9, 16);

ROR_SVE_LSL (off_1_low_ror32, svuint64_t, 31, 64);
ROR_SVE_LSL (off_1_low_ror16, svuint32_t, 15, 32);
ROR_SVE_LSL (off_1_low_ror8, svuint16_t, 7, 16);

/* Check out of bounds cases.  */

ROR_SVE_LSL (oob_ror32, svuint64_t, 65, 64);
ROR_SVE_LSL (oob_ror16, svuint32_t, 33, 32);
ROR_SVE_LSL (oob_ror8, svuint16_t, 17, 16);

/* Check zero case.  */

ROR_SVE_LSL (zero_ror32, svuint64_t, 0, 64);
ROR_SVE_LSL (zero_ror16, svuint32_t, 0, 32);
ROR_SVE_LSL (zero_ror8, svuint16_t, 0, 16);

/* { dg-final { scan-assembler-times "\trevb\t" 0 } } */
/* { dg-final { scan-assembler-times "\trevh\t" 0 } } */
/* { dg-final { scan-assembler-times "\trevw\t" 0 } } */
