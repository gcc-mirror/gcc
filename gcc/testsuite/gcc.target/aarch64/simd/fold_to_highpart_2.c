/* { dg-do compile } */
/* { dg-options "-O -march=armv9-a+bf16" } */

/* We should not use the highpart instruction unless doing so would avoid
   data movement instructions.  That is, unless at least one argument is a
   reference to the highpart of a non-constant vector.  */

#define TEST_UN_HIGHPARTS(FN, RETTYPE, INTYPE, SUFF) \
  RETTYPE test_##FN##_## SUFF ()		     \
  {                                                  \
    INTYPE a = vdupq_n_##SUFF (0x1A);		     \
    return FN##_##SUFF (vget_high_##SUFF (a));	     \
  }

#define TEST_BIN_W_HIGHPARTS(FN, RETTYPE, INTYPE, SUFF) \
  RETTYPE test_##FN##_##SUFF (RETTYPE a)		\
  {                                                     \
    INTYPE b = vdupq_n_##SUFF (0x1A);			\
    return FN##_##SUFF (a, vget_high_##SUFF (b));	\
  }

#define TEST_BIN_N_HIGHPARTS(FN, RETTYPE, INTYPE, SUFF)     \
  RETTYPE test_##FN##_##SUFF (INTYPE c)			    \
  {							    \
    INTYPE a = vdupq_n_##SUFF (0x1A);			    \
    return FN##_##SUFF (vget_high_##SUFF (a), c[1]);	    \
  }

#define TEST_TERN_N_HIGHPARTS(FN, RETTYPE, INTYPE, SUFF)      \
  RETTYPE test_##FN##_##SUFF (RETTYPE a)		      \
  {                                                           \
    INTYPE b = vdupq_n_##SUFF (0x1A);			      \
    return FN##_##SUFF (a, vget_high_##SUFF (b), b[1]);      \
  }

#define TEST_BIN_HIGHPARTS(FN, RETTYPE, INTYPE, H_INTYPE, SUFF) \
  RETTYPE test_##FN##_## SUFF (H_INTYPE b)			\
  {                                                             \
    INTYPE a = vdupq_n_##SUFF (0x1A);				\
    return FN##_##SUFF (vget_high_##SUFF (a), b);		\
  }

#define TEST_TERN_HIGHPARTS(FN, RETTYPE, INTYPE, H_INTYPE, SUFF) \
  RETTYPE test_##FN##_##SUFF (RETTYPE a, H_INTYPE b)		 \
  {                                                              \
    INTYPE c = vdupq_n_##SUFF (0x1A);				 \
    return FN##_##SUFF (a, vget_high_##SUFF (c), b);		 \
  }

#include "fold_to_highpart_1.c"


/* { dg-final { scan-assembler-not {uxtl2\t} } } */
/* { dg-final { scan-assembler-not {sxtl2\t} } } */

/* { dg-final { scan-assembler-not {umull2\t} } } */
/* { dg-final { scan-assembler-not {smull2\t} } } */
/* { dg-final { scan-assembler-not {pmull2\t} } } */

/* { dg-final { scan-assembler-not {uaddl2\t} } } */
/* { dg-final { scan-assembler-not {saddl2\t} } } */

/* { dg-final { scan-assembler-not {usubl2\t} } } */
/* { dg-final { scan-assembler-not {ssubl2\t} } } */

/* { dg-final { scan-assembler-not {uabal2\t} } } */
/* { dg-final { scan-assembler-not {sabal2\t} } } */

/* { dg-final { scan-assembler-not {uabdl2\t} } } */
/* { dg-final { scan-assembler-not {sabdl2\t} } } */

/* { dg-final { scan-assembler-not {usubw2\t} } } */
/* { dg-final { scan-assembler-not {ssubw2\t} } } */

/* { dg-final { scan-assembler-not {uaddw2\t} } } */
/* { dg-final { scan-assembler-not {saddw2\t} } } */

/* { dg-final { scan-assembler-not {umlal2\t} } } */
/* { dg-final { scan-assembler-not {smlal2\t} } } */

/* { dg-final { scan-assembler-not {umlsl2\t} } } */
/* { dg-final { scan-assembler-not {smlsl2\t} } } */

/* { dg-final { scan-assembler-not {sqdmull2\t} } } */

/* { dg-final { scan-assembler-not {sqdmlal2\t} } } */

/* { dg-final { scan-assembler-not {sqdmlsl2\t} } } */
