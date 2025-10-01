/* { dg-do compile } */
/* { dg-options "-O -march=armv9-a+bf16" } */

/* Test that we can still fold when the base type of the vector who's
   highpart we are referring to is incompatible with that of the hi
   builtin.

   Use float64x2_t as it is never INTYPE.  */

#define TEST_UN_HIGHPARTS(FN, RETTYPE, INTYPE, SUFF) \
  RETTYPE test_##FN##_##SUFF (float64x2_t a)	     \
  {                                                  \
    INTYPE x = vreinterpretq_##SUFF##_f64 (a);	     \
    return FN##_##SUFF(vget_high_##SUFF (x));	     \
  }

#define TEST_BIN_W_HIGHPARTS(FN, RETTYPE, INTYPE, SUFF)       \
  RETTYPE test_##FN##_##SUFF (RETTYPE a, float64x2_t b)	      \
  {                                                           \
    INTYPE x = vreinterpretq_##SUFF##_f64 (b);		      \
    return FN##_##SUFF (a, vget_high_##SUFF (x));	      \
  }

#define TEST_BIN_N_HIGHPARTS(FN, RETTYPE, INTYPE, SUFF)    \
  RETTYPE test_##FN##_##SUFF (float64x2_t a)		   \
  {                                                        \
    INTYPE x = vreinterpretq_##SUFF##_f64 (a);		   \
    return FN##_##SUFF (vget_high_##SUFF (x), x[1]);	   \
  }

#define TEST_TERN_N_HIGHPARTS(FN, RETTYPE, INTYPE, SUFF)         \
  RETTYPE test_##FN##_##SUFF (RETTYPE a, float64x2_t b)		 \
  {                                                              \
    INTYPE x = vreinterpretq_##SUFF##_f64 (b);			 \
    return FN##_##SUFF (a, vget_high_##SUFF (x), x[1]);	 \
  }

#define TEST_BIN_HIGHPARTS(FN, RETTYPE, INTYPE, H_INTYPE, SUFF)   \
  RETTYPE test_##FN##_##SUFF (float64x2_t a, float64x2_t b)	  \
  {                                                               \
    INTYPE x = vreinterpretq_##SUFF##_f64 (a);			  \
    INTYPE y = vreinterpretq_##SUFF##_f64 (b);			  \
    return FN##_##SUFF (vget_high_##SUFF (x),			  \
			vget_high_##SUFF (y));			  \
  }

#define TEST_TERN_HIGHPARTS(FN, RETTYPE, INTYPE, H_INTYPE, SUFF)	\
  RETTYPE test_##FN##_##SUFF (RETTYPE a, float64x2_t b, float64x2_t c)	\
  {									\
    INTYPE x = vreinterpretq_##SUFF##_f64 (b);				\
    INTYPE y = vreinterpretq_##SUFF##_f64 (c);				\
    return FN##_##SUFF (a, vget_high_## SUFF (x),                       \
			vget_high_## SUFF (y));				\
  }

#include "fold_to_highpart_1.c"

/* { dg-final { scan-assembler-times {sxtl2\t} 3} } */
/* { dg-final { scan-assembler-times {uxtl2\t} 3} } */

/* { dg-final { scan-assembler-times {smull2\t} 5} } */
/* { dg-final { scan-assembler-times {umull2\t} 5} } */
/* { dg-final { scan-assembler-times {pmull2\t} 1} } */

/* { dg-final { scan-assembler-times {saddl2\t} 3} } */
/* { dg-final { scan-assembler-times {uaddl2\t} 3} } */

/* { dg-final { scan-assembler-times {ssubl2\t} 3} } */
/* { dg-final { scan-assembler-times {usubl2\t} 3} } */

/* { dg-final { scan-assembler-times {sabal2\t} 3} } */
/* { dg-final { scan-assembler-times {uabal2\t} 3} } */

/* { dg-final { scan-assembler-times {saddw2\t} 3} } */
/* { dg-final { scan-assembler-times {uaddw2\t} 3} } */

/* { dg-final { scan-assembler-times {ssubw2\t} 3} } */
/* { dg-final { scan-assembler-times {usubw2\t} 3} } */

/* { dg-final { scan-assembler-times {sabdl2\t} 3} } */
/* { dg-final { scan-assembler-times {uabdl2\t} 3} } */

/* { dg-final { scan-assembler-times {smlal2\t} 5} } */
/* { dg-final { scan-assembler-times {umlal2\t} 5} } */

/* { dg-final { scan-assembler-times {smlsl2\t} 5} } */
/* { dg-final { scan-assembler-times {umlsl2\t} 5} } */

/* { dg-final { scan-assembler-times {sqdmull2\t} 4} } */

/* { dg-final { scan-assembler-times {sqdmlal2\t} 4} } */

/* { dg-final { scan-assembler-times {sqdmlsl2\t} 4} } */
