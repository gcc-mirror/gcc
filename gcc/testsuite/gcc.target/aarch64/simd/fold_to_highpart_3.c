/* { dg-do compile } */
/* { dg-options "-O" } */

/* PR117850 */

/* We should use the highpart instruction where doing so would avoid data
   movement instructions.  We avoid a DUP here after extending the
   VECTOR_CSTs to 128-bits.  */

#define TEST_UN_HIGHPARTS(FN, RETTYPE, INTYPE, SUFF)
#define TEST_BIN_W_HIGHPARTS(FN, RETTYPE, INTYPE, SUFF)
#define TEST_BIN_N_HIGHPARTS(FN, RETTYPE, INTYPE, SUFF)
#define TEST_TERN_N_HIGHPARTS(FN, RETTYPE, INTYPE, SUFF)

#define TEST_BIN_HIGHPART_A1(FN, RETTYPE, INTYPE, SUFF) \
  RETTYPE test_a1_##FN##_##SUFF (INTYPE a)		\
  {                                                     \
    INTYPE b = vdupq_n_##SUFF (0x1A);			\
    return FN##_##SUFF (vget_high_##SUFF (a),		\
			vget_high_##SUFF (b));		\
  }

#define TEST_BIN_HIGHPART_A2(FN, RETTYPE, INTYPE, SUFF) \
  RETTYPE test_a2_##FN##_##SUFF (INTYPE a)		\
  {                                                     \
    INTYPE b = vdupq_n_##SUFF (0x1A);			\
    return FN##_##SUFF (vget_high_##SUFF (b),		\
			vget_high_##SUFF (a));		\
  }

#define TEST_TERN_HIGHPART_A1(FN, RETTYPE, INTYPE, SUFF)    \
  RETTYPE test_a1_##FN##_##SUFF (RETTYPE a, INTYPE b)	    \
  {                                                         \
    INTYPE c = vdupq_n_##SUFF (0x1A);			    \
    return FN##_##SUFF (a, vget_high_##SUFF (b),	    \
			vget_high_##SUFF (c));		    \
  }

#define TEST_TERN_HIGHPART_A2(FN, RETTYPE, INTYPE, SUFF)    \
  RETTYPE test_a2_##FN##_##SUFF (RETTYPE a, INTYPE b)	    \
  {                                                         \
    INTYPE c = vdupq_n_##SUFF (0x1A);			    \
    return FN##_##SUFF (a, vget_high_##SUFF (c),	    \
			vget_high_##SUFF (b));		    \
  }

#define TEST_BIN_HIGHPARTS(FN, RETTYPE, INTYPE, H_INTYPE, SUFF) \
  TEST_BIN_HIGHPART_A1 (FN, RETTYPE, INTYPE, SUFF)              \
  TEST_BIN_HIGHPART_A2 (FN, RETTYPE, INTYPE, SUFF)

#define TEST_TERN_HIGHPARTS(FN, RETTYPE, INTYPE, H_INTYPE, SUFF) \
  TEST_TERN_HIGHPART_A1 (FN, RETTYPE, INTYPE, SUFF)              \
  TEST_TERN_HIGHPART_A2 (FN, RETTYPE, INTYPE, SUFF)


#include "fold_to_highpart_1.c"

/* { dg-final { scan-assembler-not {dup\t} } } */

/* { dg-final { scan-assembler-times {smull2\t} 6} } */
/* { dg-final { scan-assembler-times {umull2\t} 6} } */
/* { dg-final { scan-assembler-times {pmull2\t} 2} } */

/* { dg-final { scan-assembler-times {saddl2\t} 6} } */
/* { dg-final { scan-assembler-times {uaddl2\t} 6} } */

/* { dg-final { scan-assembler-times {ssubl2\t} 6} } */
/* { dg-final { scan-assembler-times {usubl2\t} 6} } */

/* { dg-final { scan-assembler-times {sabdl2\t} 6} } */
/* { dg-final { scan-assembler-times {uabdl2\t} 6} } */

/* { dg-final { scan-assembler-times {smlal2\t} 6} } */
/* { dg-final { scan-assembler-times {umlal2\t} 6} } */

/* { dg-final { scan-assembler-times {smlsl2\t} 6} } */
/* { dg-final { scan-assembler-times {umlsl2\t} 6} } */

/* { dg-final { scan-assembler-times {sqdmull2\t} 4} } */

/* { dg-final { scan-assembler-times {sqdmlal2\t} 4} } */

/* { dg-final { scan-assembler-times {sqdmlsl2\t} 4} } */
