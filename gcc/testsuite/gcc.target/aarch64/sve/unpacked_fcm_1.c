/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -moverride=sve_width=2048 --param=aarch64-autovec-preference=sve-only -fno-schedule-insns -fno-schedule-insns2" } */

#include <stdint.h>

#define UNLT(A, B) (!__builtin_isgreaterequal (A, B))
#define UNLE(A, B) (!__builtin_isgreater (A, B))
#define UNGT(A, B) (!__builtin_islessequal (A, B))
#define UNGE(A, B) (!__builtin_isless (A, B))
#define UNEQ(A, B) (!__builtin_islessgreater (A, B))

#define EQ(A, B) ((A) == (B))
#define NE(A, B) ((A) != (B))
#define LE(A, B) ((A) <= (B))
#define LT(A, B) ((A) < (B))
#define GE(A, B) ((A) >= (B))
#define GT(A, B) ((A) > (B))
#define ORDERED(A, B) (!__builtin_isunordered (A, B))
#define UNORDERED(A, B) (__builtin_isunordered (A, B))

#define b_i b[i]

#define TEST_FCM(TYPE0, TYPE1, CMP, RHS, COUNT)			\
  void								\
  f_##TYPE0##_##TYPE1##_##CMP##_##RHS (TYPE0 *__restrict out,	\
				       TYPE1 *__restrict a,	\
				       TYPE1 *__restrict b)	\
  {								\
    for (unsigned int i = 0; i < COUNT; i++)			\
      out[i] = CMP (a[i], RHS) ? 3 : out[i];			\
  }

#define TEST_CC_REG(CMP)		      \
  TEST_FCM (uint64_t, float, CMP, b_i, 32)    \
  TEST_FCM (uint32_t, _Float16, CMP, b_i, 64) \
  TEST_FCM (uint64_t, _Float16, CMP, b_i, 32)

#define TEST_CC_ALL(CMP)		    \
  TEST_CC_REG (CMP)			    \
  TEST_FCM (uint64_t, float, CMP, 0, 32)    \
  TEST_FCM (uint32_t, _Float16, CMP, 0, 64) \
  TEST_FCM (uint64_t, _Float16, CMP, 0, 32)


/*
** f_uint64_t_float_UNLT_b_i:
**  ...
**	ptrue	(p[0-9]+)\.d, all
**  ...
**	fcmuo	(p[0-9]+)\.s, \1/z, z[0-9]+\.s, z[0-9]+\.s
**	not	(p[0-9]+)\.b, \1/z, \2\.b
**	fcmge	p[0-9]+\.s, \3/z, z[0-9]+\.s, z[0-9]+\.s
**  ...
*/

/*
** f_uint32_t__Float16_UNLT_b_i:
**  ...
**	ptrue	(p[0-9]+)\.s, all
**  ...
**	fcmuo	(p[0-9]+)\.h, \1/z, z[0-9]+\.h, z[0-9]+\.h
**	not	(p[0-9]+)\.b, \1/z, \2\.b
**	fcmge	p[0-9]+\.h, \3/z, z[0-9]+\.h, z[0-9]+\.h
**  ...
*/

/*
** f_uint64_t__Float16_UNLT_b_i:
**  ...
**	ptrue	(p[0-9]+)\.d, all
**  ...
**	fcmuo	(p[0-9]+)\.h, \1/z, z[0-9]+\.h, z[0-9]+\.h
**	not	(p[0-9]+)\.b, \1/z, \2\.b
**	fcmge	p[0-9]+\.h, \3/z, z[0-9]+\.h, z[0-9]+\.h
**  ...
*/
TEST_CC_REG (UNLT)

/*
** f_uint64_t_float_UNLE_b_i:
**  ...
**	ptrue	(p[0-9]+)\.d, all
**  ...
**	fcmuo	(p[0-9]+)\.s, \1/z, z[0-9]+\.s, z[0-9]+\.s
**	not	(p[0-9]+)\.b, \1/z, \2\.b
**	fcmgt	p[0-9]+\.s, \3/z, z[0-9]+\.s, z[0-9]+\.s
**  ...
*/

/*
** f_uint32_t__Float16_UNLE_b_i:
**  ...
**	ptrue	(p[0-9]+)\.s, all
**  ...
**	fcmuo	(p[0-9]+)\.h, \1/z, z[0-9]+\.h, z[0-9]+\.h
**	not	(p[0-9]+)\.b, \1/z, \2\.b
**	fcmgt	p[0-9]+\.h, \3/z, z[0-9]+\.h, z[0-9]+\.h
**  ...
*/

/*
** f_uint64_t__Float16_UNLE_b_i:
**  ...
**	ptrue	(p[0-9]+)\.d, all
**  ...
**	fcmuo	(p[0-9]+)\.h, \1/z, z[0-9]+\.h, z[0-9]+\.h
**	not	(p[0-9]+)\.b, \1/z, \2\.b
**	fcmgt	p[0-9]+\.h, \3/z, z[0-9]+\.h, z[0-9]+\.h
**  ...
*/
TEST_CC_REG (UNLE)

/*
** f_uint64_t_float_UNGT_b_i:
**  ...
**	ptrue	(p[0-9]+)\.d, all
**  ...
**	fcmuo	(p[0-9]+)\.s, \1/z, z[0-9]+\.s, z[0-9]+\.s
**	not	(p[0-9]+)\.b, \1/z, \2\.b
**	fcmle	p[0-9]+\.s, \3/z, z[0-9]+\.s, z[0-9]+\.s
**  ...
*/

/*
** f_uint32_t__Float16_UNGT_b_i:
**  ...
**	ptrue	(p[0-9]+)\.s, all
**  ...
**	fcmuo	(p[0-9]+)\.h, \1/z, z[0-9]+\.h, z[0-9]+\.h
**	not	(p[0-9]+)\.b, \1/z, \2\.b
**	fcmle	p[0-9]+\.h, \3/z, z[0-9]+\.h, z[0-9]+\.h
**  ...
*/

/*
** f_uint64_t__Float16_UNGT_b_i:
**  ...
**	ptrue	(p[0-9]+)\.d, all
**  ...
**	fcmuo	(p[0-9]+)\.h, \1/z, z[0-9]+\.h, z[0-9]+\.h
**	not	(p[0-9]+)\.b, \1/z, \2\.b
**	fcmle	p[0-9]+\.h, \3/z, z[0-9]+\.h, z[0-9]+\.h
**  ...
*/
TEST_CC_REG (UNGT)

/*
** f_uint64_t_float_UNGE_b_i:
**  ...
**	ptrue	(p[0-9]+)\.d, all
**  ...
**	fcmuo	(p[0-9]+)\.s, \1/z, z[0-9]+\.s, z[0-9]+\.s
**	not	(p[0-9]+)\.b, \1/z, \2\.b
**	fcmlt	p[0-9]+\.s, \3/z, z[0-9]+\.s, z[0-9]+\.s
**  ...
*/

/*
** f_uint32_t__Float16_UNGE_b_i:
**  ...
**	ptrue	(p[0-9]+)\.s, all
**  ...
**	fcmuo	(p[0-9]+)\.h, \1/z, z[0-9]+\.h, z[0-9]+\.h
**	not	(p[0-9]+)\.b, \1/z, \2\.b
**	fcmlt	p[0-9]+\.h, \3/z, z[0-9]+\.h, z[0-9]+\.h
**  ...
*/

/*
** f_uint64_t__Float16_UNGE_b_i:
**  ...
**	ptrue	(p[0-9]+)\.d, all
**  ...
**	fcmuo	(p[0-9]+)\.h, \1/z, z[0-9]+\.h, z[0-9]+\.h
**	not	(p[0-9]+)\.b, \1/z, \2\.b
**	fcmlt	p[0-9]+\.h, \3/z, z[0-9]+\.h, z[0-9]+\.h
**  ...
*/
TEST_CC_REG (UNGE)

/*
** f_uint64_t_float_UNEQ_b_i:
**  ...
**	ptrue	(p[0-9]+)\.d, all
**  ...
**	fcmuo	(p[0-9]+)\.s, \1/z, z[0-9]+\.s, z[0-9]+\.s
**	not	(p[0-9]+)\.b, \1/z, \2\.b
**	fcmne	p[0-9]+\.s, \3/z, z[0-9]+\.s, z[0-9]+\.s
**  ...
*/

/*
** f_uint32_t__Float16_UNEQ_b_i:
**  ...
**	ptrue	(p[0-9]+)\.s, all
**  ...
**	fcmuo	(p[0-9]+)\.h, \1/z, z[0-9]+\.h, z[0-9]+\.h
**	not	(p[0-9]+)\.b, \1/z, \2\.b
**	fcmne	p[0-9]+\.h, \3/z, z[0-9]+\.h, z[0-9]+\.h
**  ...
*/

/*
** f_uint64_t__Float16_UNEQ_b_i:
**  ...
**	ptrue	(p[0-9]+)\.d, all
**  ...
**	fcmuo	(p[0-9]+)\.h, \1/z, z[0-9]+\.h, z[0-9]+\.h
**	not	(p[0-9]+)\.b, \1/z, \2\.b
**	fcmne	p[0-9]+\.h, \3/z, z[0-9]+\.h, z[0-9]+\.h
**  ...
*/
TEST_CC_REG (UNEQ)

/*
** f_uint64_t_float_EQ_b_i:
**  ...
**	ptrue	(p[0-9]+)\.d, all
**  ...
**	fcmeq	p[0-9]+\.s, \1/z, z[0-9]+\.s, z[0-9]+\.s
**  ...
*/

/*
** f_uint32_t__Float16_EQ_b_i:
**  ...
**	ptrue	(p[0-9]+)\.s, all
**  ...
**	fcmeq	p[0-9]+\.h, \1/z, z[0-9]+\.h, z[0-9]+\.h
**  ...
*/

/*
** f_uint64_t__Float16_EQ_b_i:
**  ...
**	ptrue	(p[0-9]+)\.d, all
**  ...
**	fcmeq	p[0-9]+\.h, \1/z, z[0-9]+\.h, z[0-9]+\.h
**  ...
*/

/*
** f_uint64_t_float_EQ_0:
**  ...
**	ptrue	(p[0-9]+)\.d, all
**  ...
**	fcmeq	p[0-9]+\.s, \1/z, z[0-9]+\.s, #0.0
**  ...
*/

/*
** f_uint32_t__Float16_EQ_0:
**  ...
**	ptrue	(p[0-9]+)\.s, all
**  ...
**	fcmeq	p[0-9]+\.h, \1/z, z[0-9]+\.h, #0.0
**  ...
*/

/*
** f_uint64_t__Float16_EQ_0:
**  ...
**	ptrue	(p[0-9]+)\.d, all
**  ...
**	fcmeq	p[0-9]+\.h, \1/z, z[0-9]+\.h, #0.0
**  ...
*/
TEST_CC_ALL (EQ)

/*
** f_uint64_t_float_NE_b_i:
**  ...
**	ptrue	(p[0-9]+)\.d, all
**  ...
**	fcmne	p[0-9]+\.s, \1/z, z[0-9]+\.s, z[0-9]+\.s
**  ...
*/

/*
** f_uint32_t__Float16_NE_b_i:
**  ...
**	ptrue	(p[0-9]+)\.s, all
**  ...
**	fcmne	p[0-9]+\.h, \1/z, z[0-9]+\.h, z[0-9]+\.h
**  ...
*/

/*
** f_uint64_t__Float16_NE_b_i:
**  ...
**	ptrue	(p[0-9]+)\.d, all
**  ...
**	fcmne	p[0-9]+\.h, \1/z, z[0-9]+\.h, z[0-9]+\.h
**  ...
*/

/*
** f_uint64_t_float_NE_0:
**  ...
**	ptrue	(p[0-9]+)\.d, all
**  ...
**	fcmne	p[0-9]+\.s, \1/z, z[0-9]+\.s, #0.0
**  ...
*/

/*
** f_uint32_t__Float16_NE_0:
**  ...
**	ptrue	(p[0-9]+)\.s, all
**  ...
**	fcmne	p[0-9]+\.h, \1/z, z[0-9]+\.h, #0.0
**  ...
*/

/*
** f_uint64_t__Float16_NE_0:
**  ...
**	ptrue	(p[0-9]+)\.d, all
**  ...
**	fcmne	p[0-9]+\.h, \1/z, z[0-9]+\.h, #0.0
**  ...
*/
TEST_CC_ALL (NE)

/*
** f_uint64_t_float_LE_b_i:
**  ...
**	ptrue	(p[0-9]+)\.d, all
**  ...
**	fcmle	p[0-9]+\.s, \1/z, z[0-9]+\.s, z[0-9]+\.s
**  ...
*/

/*
** f_uint32_t__Float16_LE_b_i:
**  ...
**	ptrue	(p[0-9]+)\.s, all
**  ...
**	fcmle	p[0-9]+\.h, \1/z, z[0-9]+\.h, z[0-9]+\.h
**  ...
*/

/*
** f_uint64_t__Float16_LE_b_i:
**  ...
**	ptrue	(p[0-9]+)\.d, all
**  ...
**	fcmle	p[0-9]+\.h, \1/z, z[0-9]+\.h, z[0-9]+\.h
**  ...
*/

/*
** f_uint64_t_float_LE_0:
**  ...
**	ptrue	(p[0-9]+)\.d, all
**  ...
**	fcmle	p[0-9]+\.s, \1/z, z[0-9]+\.s, #0.0
**  ...
*/

/*
** f_uint32_t__Float16_LE_0:
**  ...
**	ptrue	(p[0-9]+)\.s, all
**  ...
**	fcmle	p[0-9]+\.h, \1/z, z[0-9]+\.h, #0.0
**  ...
*/

/*
** f_uint64_t__Float16_LE_0:
**  ...
**	ptrue	(p[0-9]+)\.d, all
**  ...
**	fcmle	p[0-9]+\.h, \1/z, z[0-9]+\.h, #0.0
**  ...
*/
TEST_CC_ALL (LE)

/*
** f_uint64_t_float_LT_b_i:
**  ...
**	ptrue	(p[0-9]+)\.d, all
**  ...
**	fcmlt	p[0-9]+\.s, \1/z, z[0-9]+\.s, z[0-9]+\.s
**  ...
*/

/*
** f_uint32_t__Float16_LT_b_i:
**  ...
**	ptrue	(p[0-9]+)\.s, all
**  ...
**	fcmlt	p[0-9]+\.h, \1/z, z[0-9]+\.h, z[0-9]+\.h
**  ...
*/

/*
** f_uint64_t__Float16_LT_b_i:
**  ...
**	ptrue	(p[0-9]+)\.d, all
**  ...
**	fcmlt	p[0-9]+\.h, \1/z, z[0-9]+\.h, z[0-9]+\.h
**  ...
*/

/*
** f_uint64_t_float_LT_0:
**  ...
**	ptrue	(p[0-9]+)\.d, all
**  ...
**	fcmlt	p[0-9]+\.s, \1/z, z[0-9]+\.s, #0.0
**  ...
*/

/*
** f_uint32_t__Float16_LT_0:
**  ...
**	ptrue	(p[0-9]+)\.s, all
**  ...
**	fcmlt	p[0-9]+\.h, \1/z, z[0-9]+\.h, #0.0
**  ...
*/

/*
** f_uint64_t__Float16_LT_0:
**  ...
**	ptrue	(p[0-9]+)\.d, all
**  ...
**	fcmlt	p[0-9]+\.h, \1/z, z[0-9]+\.h, #0.0
**  ...
*/
TEST_CC_ALL (LT)

/*
** f_uint64_t_float_GE_b_i:
**  ...
**	ptrue	(p[0-9]+)\.d, all
**  ...
**	fcmge	p[0-9]+\.s, \1/z, z[0-9]+\.s, z[0-9]+\.s
**  ...
*/

/*
** f_uint32_t__Float16_GE_b_i:
**  ...
**	ptrue	(p[0-9]+)\.s, all
**  ...
**	fcmge	p[0-9]+\.h, \1/z, z[0-9]+\.h, z[0-9]+\.h
**  ...
*/

/*
** f_uint64_t__Float16_GE_b_i:
**  ...
**	ptrue	(p[0-9]+)\.d, all
**  ...
**	fcmge	p[0-9]+\.h, \1/z, z[0-9]+\.h, z[0-9]+\.h
**  ...
*/

/*
** f_uint64_t_float_GE_0:
**  ...
**	ptrue	(p[0-9]+)\.d, all
**  ...
**	fcmge	p[0-9]+\.s, \1/z, z[0-9]+\.s, #0.0
**  ...
*/

/*
** f_uint32_t__Float16_GE_0:
**  ...
**	ptrue	(p[0-9]+)\.s, all
**  ...
**	fcmge	p[0-9]+\.h, \1/z, z[0-9]+\.h, #0.0
**  ...
*/

/*
** f_uint64_t__Float16_GE_0:
**  ...
**	ptrue	(p[0-9]+)\.d, all
**  ...
**	fcmge	p[0-9]+\.h, \1/z, z[0-9]+\.h, #0.0
**  ...
*/
TEST_CC_ALL (GE)

/*
** f_uint64_t_float_GT_b_i:
**  ...
**	ptrue	(p[0-9]+)\.d, all
**  ...
**	fcmgt	p[0-9]+\.s, \1/z, z[0-9]+\.s, z[0-9]+\.s
**  ...
*/

/*
** f_uint32_t__Float16_GT_b_i:
**  ...
**	ptrue	(p[0-9]+)\.s, all
**  ...
**	fcmgt	p[0-9]+\.h, \1/z, z[0-9]+\.h, z[0-9]+\.h
**  ...
*/

/*
** f_uint64_t__Float16_GT_b_i:
**  ...
**	ptrue	(p[0-9]+)\.d, all
**  ...
**	fcmgt	p[0-9]+\.h, \1/z, z[0-9]+\.h, z[0-9]+\.h
**  ...
*/

/*
** f_uint64_t_float_GT_0:
**  ...
**	ptrue	(p[0-9]+)\.d, all
**  ...
**	fcmgt	p[0-9]+\.s, \1/z, z[0-9]+\.s, #0.0
**  ...
*/

/*
** f_uint32_t__Float16_GT_0:
**  ...
**	ptrue	(p[0-9]+)\.s, all
**  ...
**	fcmgt	p[0-9]+\.h, \1/z, z[0-9]+\.h, #0.0
**  ...
*/

/*
** f_uint64_t__Float16_GT_0:
**  ...
**	ptrue	(p[0-9]+)\.d, all
**  ...
**	fcmgt	p[0-9]+\.h, \1/z, z[0-9]+\.h, #0.0
**  ...
*/
TEST_CC_ALL (GT)

/*
** f_uint64_t_float_ORDERED_b_i:
**  ...
**	ptrue	(p[0-9]+)\.d, all
**  ...
**	fcmuo	p[0-9]+\.s, \1/z, z[0-9]+\.s, z[0-9]+\.s
**  ...
*/

/*
** f_uint32_t__Float16_ORDERED_b_i:
**  ...
**	ptrue	(p[0-9]+)\.s, all
**  ...
**	fcmuo	p[0-9]+\.h, \1/z, z[0-9]+\.h, z[0-9]+\.h
**  ...
*/

/*
** f_uint64_t__Float16_ORDERED_b_i:
**  ...
**	ptrue	(p[0-9]+)\.d, all
**  ...
**	fcmuo	p[0-9]+\.h, \1/z, z[0-9]+\.h, z[0-9]+\.h
**  ...
*/
TEST_CC_REG (ORDERED)

/*
** f_uint64_t_float_UNORDERED_b_i:
**  ...
**	ptrue	(p[0-9]+)\.d, all
**  ...
**	fcmuo	p[0-9]+\.s, \1/z, z[0-9]+\.s, z[0-9]+\.s
**  ...
*/

/*
** f_uint32_t__Float16_UNORDERED_b_i:
**  ...
**	ptrue	(p[0-9]+)\.s, all
**  ...
**	fcmuo	p[0-9]+\.h, \1/z, z[0-9]+\.h, z[0-9]+\.h
**  ...
*/

/*
** f_uint64_t__Float16_UNORDERED_b_i:
**  ...
**	ptrue	(p[0-9]+)\.d, all
**  ...
**	fcmuo	p[0-9]+\.h, \1/z, z[0-9]+\.h, z[0-9]+\.h
**  ...
*/
TEST_CC_REG (UNORDERED)


/* { dg-final { check-function-bodies "**" "" ""} } */
