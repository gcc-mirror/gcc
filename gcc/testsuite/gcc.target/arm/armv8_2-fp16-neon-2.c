/* { dg-do compile }  */
/* { dg-require-effective-target arm_v8_2a_fp16_neon_ok }  */
/* { dg-options "-O2 -ffast-math" }  */
/* { dg-add-options arm_v8_2a_fp16_neon }  */

/* Test instructions generated for the FP16 vector intrinsics with
   -ffast-math */

#include <arm_neon.h>

#define MSTRCAT(L, str)	L##str

#define UNOP_TEST(insn)				\
  float16x4_t					\
  MSTRCAT (test_##insn, _16x4) (float16x4_t a)	\
  {						\
    return MSTRCAT (insn, _f16) (a);		\
  }						\
  float16x8_t					\
  MSTRCAT (test_##insn, _16x8) (float16x8_t a)	\
  {						\
    return MSTRCAT (insn, q_f16) (a);		\
  }

#define BINOP_TEST(insn)					\
  float16x4_t							\
  MSTRCAT (test_##insn, _16x4) (float16x4_t a, float16x4_t b)	\
  {								\
    return MSTRCAT (insn, _f16) (a, b);				\
  }								\
  float16x8_t							\
  MSTRCAT (test_##insn, _16x8) (float16x8_t a, float16x8_t b)	\
  {								\
    return MSTRCAT (insn, q_f16) (a, b);			\
  }

#define BINOP_LANE_TEST(insn, I)					\
  float16x4_t								\
  MSTRCAT (test_##insn##_lane, _16x4) (float16x4_t a, float16x4_t b)	\
  {									\
    return MSTRCAT (insn, _lane_f16) (a, b, I);				\
  }									\
  float16x8_t								\
  MSTRCAT (test_##insn##_lane, _16x8) (float16x8_t a, float16x4_t b)	\
  {									\
    return MSTRCAT (insn, q_lane_f16) (a, b, I);			\
  }

#define BINOP_LANEQ_TEST(insn, I)					\
  float16x4_t								\
  MSTRCAT (test_##insn##_laneq, _16x4) (float16x4_t a, float16x8_t b)	\
  {									\
    return MSTRCAT (insn, _laneq_f16) (a, b, I);			\
  }									\
  float16x8_t								\
  MSTRCAT (test_##insn##_laneq, _16x8) (float16x8_t a, float16x8_t b)	\
  {									\
    return MSTRCAT (insn, q_laneq_f16) (a, b, I);			\
  }									\

#define BINOP_N_TEST(insn)					\
  float16x4_t							\
  MSTRCAT (test_##insn##_n, _16x4) (float16x4_t a, float16_t b)	\
  {								\
    return MSTRCAT (insn, _n_f16) (a, b);			\
  }								\
  float16x8_t							\
  MSTRCAT (test_##insn##_n, _16x8) (float16x8_t a, float16_t b)	\
  {								\
    return MSTRCAT (insn, q_n_f16) (a, b);			\
  }

#define TERNOP_TEST(insn)						\
  float16_t								\
  MSTRCAT (test_##insn, _16) (float16_t a, float16_t b, float16_t c)	\
  {									\
    return MSTRCAT (insn, h_f16) (a, b, c);				\
  }									\
  float16x4_t								\
  MSTRCAT (test_##insn, _16x4) (float16x4_t a, float16x4_t b,		\
			       float16x4_t c)				\
  {									\
    return MSTRCAT (insn, _f16) (a, b, c);				\
  }									\
  float16x8_t								\
  MSTRCAT (test_##insn, _16x8) (float16x8_t a, float16x8_t b,		\
			       float16x8_t c)				\
  {									\
    return MSTRCAT (insn, q_f16) (a, b, c);				\
  }

#define VCMP1_TEST(insn)			\
  uint16x4_t					\
  MSTRCAT (test_##insn, _16x4) (float16x4_t a)	\
  {						\
    return MSTRCAT (insn, _f16) (a);		\
  }						\
  uint16x8_t					\
  MSTRCAT (test_##insn, _16x8) (float16x8_t a)	\
  {						\
    return MSTRCAT (insn, q_f16) (a);		\
  }

#define VCMP2_TEST(insn)					\
  uint16x4_t							\
  MSTRCAT (test_##insn, _16x4) (float16x4_t a, float16x4_t b)	\
  {								\
    return MSTRCAT (insn, _f16) (a, b);				\
  }								\
  uint16x8_t							\
  MSTRCAT (test_##insn, _16x8) (float16x8_t a, float16x8_t b)	\
  {								\
    return MSTRCAT (insn, q_f16) (a, b);			\
  }

#define VCVT_TEST(insn, TY, TO, FR)			\
  MSTRCAT (TO, 16x4_t)					\
  MSTRCAT (test_##insn, TY) (MSTRCAT (FR, 16x4_t) a)	\
  {							\
    return MSTRCAT (insn, TY) (a);			\
  }							\
  MSTRCAT (TO, 16x8_t)					\
  MSTRCAT (test_##insn##_q, TY) (MSTRCAT (FR, 16x8_t) a)	\
  {							\
    return MSTRCAT (insn, q##TY) (a);			\
  }

#define VCVT_N_TEST(insn, TY, TO, FR)			\
  MSTRCAT (TO, 16x4_t)					\
  MSTRCAT (test_##insn##_n, TY) (MSTRCAT (FR, 16x4_t) a)	\
  {							\
    return MSTRCAT (insn, _n##TY) (a, 1);		\
  }							\
  MSTRCAT (TO, 16x8_t)					\
  MSTRCAT (test_##insn##_n_q, TY) (MSTRCAT (FR, 16x8_t) a)	\
  {							\
    return MSTRCAT (insn, q_n##TY) (a, 1);		\
  }

VCMP1_TEST (vceqz)
/* { dg-final { scan-assembler-times {vceq\.f16\td[0-9]+, d[0-9]+, #0} 1 } }  */
/* { dg-final { scan-assembler-times {vceq\.f16\tq[0-9]+, q[0-9]+, #0} 1 } }  */

VCMP1_TEST (vcgtz)
/* { dg-final { scan-assembler-times {vcgt\.f16\td[0-9]+, d[0-9]+, #0} 1 } }  */
/* { dg-final { scan-assembler-times {vceq\.f16\tq[0-9]+, q[0-9]+, #0} 1 } }  */

VCMP1_TEST (vcgez)
/* { dg-final { scan-assembler-times {vcge\.f16\td[0-9]+, d[0-9]+, #0} 1 } }  */
/* { dg-final { scan-assembler-times {vcge\.f16\tq[0-9]+, q[0-9]+, #0} 1 } }  */

VCMP1_TEST (vcltz)
/* { dg-final { scan-assembler-times {vclt.f16\td[0-9]+, d[0-9]+, #0} 1 } }  */
/* { dg-final { scan-assembler-times {vclt.f16\tq[0-9]+, q[0-9]+, #0} 1 } }  */

VCMP1_TEST (vclez)
/* { dg-final { scan-assembler-times {vcle\.f16\td[0-9]+, d[0-9]+, #0} 1 } }  */
/* { dg-final { scan-assembler-times {vcle\.f16\tq[0-9]+, q[0-9]+, #0} 1 } }  */

VCVT_TEST (vcvt, _f16_s16, float, int)
VCVT_N_TEST (vcvt, _f16_s16, float, int)
/* { dg-final { scan-assembler-times {vcvt\.f16\.s16\td[0-9]+, d[0-9]+} 2 } }
   { dg-final { scan-assembler-times {vcvt\.f16\.s16\tq[0-9]+, q[0-9]+} 2 } }
   { dg-final { scan-assembler-times {vcvt\.f16\.s16\td[0-9]+, d[0-9]+, #1} 1 } }
   { dg-final { scan-assembler-times {vcvt\.f16\.s16\tq[0-9]+, q[0-9]+, #1} 1 } }  */

VCVT_TEST (vcvt, _f16_u16, float, uint)
VCVT_N_TEST (vcvt, _f16_u16, float, uint)
/* { dg-final { scan-assembler-times {vcvt\.f16\.u16\td[0-9]+, d[0-9]+} 2 } }
   { dg-final { scan-assembler-times {vcvt\.f16\.u16\tq[0-9]+, q[0-9]+} 2 } }
   { dg-final { scan-assembler-times {vcvt\.f16\.u16\td[0-9]+, d[0-9]+, #1} 1 } }
   { dg-final { scan-assembler-times {vcvt\.f16\.u16\tq[0-9]+, q[0-9]+, #1} 1 } }  */

VCVT_TEST (vcvt, _s16_f16, int, float)
VCVT_N_TEST (vcvt, _s16_f16, int, float)
/* { dg-final { scan-assembler-times {vcvt\.s16\.f16\td[0-9]+, d[0-9]+} 2 } }
   { dg-final { scan-assembler-times {vcvt\.s16\.f16\tq[0-9]+, q[0-9]+} 2 } }
   { dg-final { scan-assembler-times {vcvt\.s16\.f16\td[0-9]+, d[0-9]+, #1} 1 } }
   { dg-final { scan-assembler-times {vcvt\.s16\.f16\tq[0-9]+, q[0-9]+, #1} 1 } }  */

VCVT_TEST (vcvt, _u16_f16, uint, float)
VCVT_N_TEST (vcvt, _u16_f16, uint, float)
/* { dg-final { scan-assembler-times {vcvt\.u16\.f16\td[0-9]+, d[0-9]+} 2 } }
   { dg-final { scan-assembler-times {vcvt\.u16\.f16\tq[0-9]+, q[0-9]+} 2 } }
   { dg-final { scan-assembler-times {vcvt\.u16\.f16\td[0-9]+, d[0-9]+, #1} 1 } }
   { dg-final { scan-assembler-times {vcvt\.u16\.f16\tq[0-9]+, q[0-9]+, #1} 1 } }  */

VCVT_TEST (vcvta, _s16_f16, int, float)
/* { dg-final { scan-assembler-times {vcvta\.s16\.f16\td[0-9]+, d[0-9]+} 1 } }
   { dg-final { scan-assembler-times {vcvta\.s16\.f16\tq[0-9]+, q[0-9]+} 1 } }
*/

VCVT_TEST (vcvta, _u16_f16, uint, float)
/* { dg-final { scan-assembler-times {vcvta\.u16\.f16\td[0-9]+, d[0-9]+} 1 } }
   { dg-final { scan-assembler-times {vcvta\.u16\.f16\tq[0-9]+, q[0-9]+} 1 } }
*/

VCVT_TEST (vcvtm, _s16_f16, int, float)
/* { dg-final { scan-assembler-times {vcvtm\.s16\.f16\td[0-9]+, d[0-9]+} 1 } }
   { dg-final { scan-assembler-times {vcvtm\.s16\.f16\tq[0-9]+, q[0-9]+} 1 } }
*/

VCVT_TEST (vcvtm, _u16_f16, uint, float)
/* { dg-final { scan-assembler-times {vcvtm\.u16\.f16\td[0-9]+, d[0-9]+} 1 } }
   { dg-final { scan-assembler-times {vcvtm\.u16\.f16\tq[0-9]+, q[0-9]+} 1 } }
*/

VCVT_TEST (vcvtn, _s16_f16, int, float)
/* { dg-final { scan-assembler-times {vcvtn\.s16\.f16\td[0-9]+, d[0-9]+} 1 } }
   { dg-final { scan-assembler-times {vcvtn\.s16\.f16\tq[0-9]+, q[0-9]+} 1 } }
*/

VCVT_TEST (vcvtn, _u16_f16, uint, float)
/* { dg-final { scan-assembler-times {vcvtn\.u16\.f16\td[0-9]+, d[0-9]+} 1 } }
   { dg-final { scan-assembler-times {vcvtn\.u16\.f16\tq[0-9]+, q[0-9]+} 1 } }
*/

VCVT_TEST (vcvtp, _s16_f16, int, float)
/* { dg-final { scan-assembler-times {vcvtp\.s16\.f16\td[0-9]+, d[0-9]+} 1 } }
   { dg-final { scan-assembler-times {vcvtp\.s16\.f16\tq[0-9]+, q[0-9]+} 1 } }
*/

VCVT_TEST (vcvtp, _u16_f16, uint, float)
/* { dg-final { scan-assembler-times {vcvtp\.u16\.f16\td[0-9]+, d[0-9]+} 1 } }
   { dg-final { scan-assembler-times {vcvtp\.u16\.f16\tq[0-9]+, q[0-9]+} 1 } }
*/

UNOP_TEST (vabs)
/* { dg-final { scan-assembler-times {vabs\.f16\td[0-9]+, d[0-9]+} 1 } }
   { dg-final { scan-assembler-times {vabs\.f16\tq[0-9]+, q[0-9]+} 1 } }  */

UNOP_TEST (vneg)
/* { dg-final { scan-assembler-times {vneg\.f16\td[0-9]+, d[0-9]+} 1 } }
   { dg-final { scan-assembler-times {vneg\.f16\tq[0-9]+, q[0-9]+} 1 } }  */

UNOP_TEST (vrecpe)
/* { dg-final { scan-assembler-times {vrecpe\.f16\td[0-9]+, d[0-9]+} 1 } }
   { dg-final { scan-assembler-times {vrecpe\.f16\tq[0-9]+, q[0-9]+} 1 } }  */

UNOP_TEST (vrnd)
/* { dg-final { scan-assembler-times {vrintz\.f16\td[0-9]+, d[0-9]+} 1 } }
   { dg-final { scan-assembler-times {vrintz\.f16\tq[0-9]+, q[0-9]+} 1 } }  */

UNOP_TEST (vrnda)
/* { dg-final { scan-assembler-times {vrinta\.f16\td[0-9]+, d[0-9]+} 1 } }
   { dg-final { scan-assembler-times {vrinta\.f16\tq[0-9]+, q[0-9]+} 1 } }  */

UNOP_TEST (vrndm)
/* { dg-final { scan-assembler-times {vrintm\.f16\td[0-9]+, d[0-9]+} 1 } }
   { dg-final { scan-assembler-times {vrintm\.f16\tq[0-9]+, q[0-9]+} 1 } }  */

UNOP_TEST (vrndn)
/* { dg-final { scan-assembler-times {vrintn\.f16\td[0-9]+, d[0-9]+} 1 } }
   { dg-final { scan-assembler-times {vrintn\.f16\tq[0-9]+, q[0-9]+} 1 } }  */

UNOP_TEST (vrndp)
/* { dg-final { scan-assembler-times {vrintp\.f16\td[0-9]+, d[0-9]+} 1 } }
   { dg-final { scan-assembler-times {vrintp\.f16\tq[0-9]+, q[0-9]+} 1 } }  */

UNOP_TEST (vrndx)
/* { dg-final { scan-assembler-times {vrintx\.f16\td[0-9]+, d[0-9]+} 1 } }
   { dg-final { scan-assembler-times {vrintx\.f16\tq[0-9]+, q[0-9]+} 1 } }  */

UNOP_TEST (vrsqrte)
/* { dg-final { scan-assembler-times {vrsqrte\.f16\td[0-9]+, d[0-9]+} 1 } }
   { dg-final { scan-assembler-times {vrsqrte\.f16\tq[0-9]+, q[0-9]+} 1 } }  */

BINOP_TEST (vadd)
/* { dg-final { scan-assembler-times {vadd\.f16\td[0-9]+, d[0-9]+, d[0-9]+} 1 } }
   { dg-final { scan-assembler-times {vadd\.f16\tq[0-9]+, q[0-9]+, q[0-9]+} 1 } }  */

BINOP_TEST (vabd)
/* { dg-final { scan-assembler-times {vabd\.f16\td[0-9]+, d[0-9]+, d[0-9]+} 1 } }
   { dg-final { scan-assembler-times {vabd\.f16\tq[0-9]+, q[0-9]+, q[0-9]+} 1 } }  */

VCMP2_TEST (vcage)
/* { dg-final { scan-assembler-times {vacge\.f16\td[0-9]+, d[0-9]+, d[0-9]+} 1 } }
   { dg-final { scan-assembler-times {vacge\.f16\tq[0-9]+, q[0-9]+, q[0-9]+} 1 } }  */

VCMP2_TEST (vcagt)
/* { dg-final { scan-assembler-times {vacgt\.f16\td[0-9]+, d[0-9]+, d[0-9]+} 1 } }
   { dg-final { scan-assembler-times {vacgt\.f16\tq[0-9]+, q[0-9]+, q[0-9]+} 1 } }  */

VCMP2_TEST (vcale)
/* { dg-final { scan-assembler-times {vacle\.f16\td[0-9]+, d[0-9]+, d[0-9]+} 1 } }
   { dg-final { scan-assembler-times {vacle\.f16\tq[0-9]+, q[0-9]+, q[0-9]+} 1 } }  */

VCMP2_TEST (vcalt)
/* { dg-final { scan-assembler-times {vaclt\.f16\td[0-9]+, d[0-9]+, d[0-9]+} 1 } }
   { dg-final { scan-assembler-times {vaclt\.f16\tq[0-9]+, q[0-9]+, q[0-9]+} 1 } }  */

VCMP2_TEST (vceq)
/* { dg-final { scan-assembler-times {vceq\.f16\td[0-9]+, d[0-9]+, d[0-9]+} 1 } }
   { dg-final { scan-assembler-times {vceq\.f16\tq[0-9]+, q[0-9]+, q[0-9]+} 1 } }  */

VCMP2_TEST (vcge)
/* { dg-final { scan-assembler-times {vcge\.f16\td[0-9]+, d[0-9]+, d[0-9]+} 1 } }
   { dg-final { scan-assembler-times {vcge\.f16\tq[0-9]+, q[0-9]+, q[0-9]+} 1 } }  */

VCMP2_TEST (vcgt)
/* { dg-final { scan-assembler-times {vcgt\.f16\td[0-9]+, d[0-9]+, d[0-9]+} 1 } }
   { dg-final { scan-assembler-times {vcgt\.f16\tq[0-9]+, q[0-9]+, q[0-9]+} 1 } }  */

VCMP2_TEST (vcle)
/* { dg-final { scan-assembler-times {vcle\.f16\td[0-9]+, d[0-9]+, d[0-9]+} 1 } }
   { dg-final { scan-assembler-times {vcle\.f16\tq[0-9]+, q[0-9]+, q[0-9]+} 1 } }  */

VCMP2_TEST (vclt)
/* { dg-final { scan-assembler-times {vclt\.f16\td[0-9]+, d[0-9]+, d[0-9]+} 1 } }
   { dg-final { scan-assembler-times {vclt\.f16\tq[0-9]+, q[0-9]+, q[0-9]+} 1 } }  */

BINOP_TEST (vmax)
/* { dg-final { scan-assembler-times {vmax\.f16\td[0-9]+, d[0-9]+, d[0-9]+} 1 } }
   { dg-final { scan-assembler-times {vmax\.f16\tq[0-9]+, q[0-9]+, q[0-9]+} 1 } }  */

BINOP_TEST (vmin)
/* { dg-final { scan-assembler-times {vmin\.f16\td[0-9]+, d[0-9]+, d[0-9]+} 1 } }
   { dg-final { scan-assembler-times {vmin\.f16\tq[0-9]+, q[0-9]+, q[0-9]+} 1 } }  */

BINOP_TEST (vmaxnm)
/* { dg-final { scan-assembler-times {vmaxnm\.f16\td[0-9]+, d[0-9]+, d[0-9]+} 1 } }
  { dg-final { scan-assembler-times {vmaxnm\.f16\tq[0-9]+, q[0-9]+, q[0-9]+} 1 } }  */

BINOP_TEST (vminnm)
/* { dg-final { scan-assembler-times {vminnm\.f16\td[0-9]+, d[0-9]+, d[0-9]+} 1 } }
  { dg-final { scan-assembler-times {vminnm\.f16\tq[0-9]+, q[0-9]+, q[0-9]+} 1 } }  */

BINOP_TEST (vmul)
/* { dg-final { scan-assembler-times {vmul\.f16\td[0-9]+, d[0-9]+, d[0-9]+} 3 } }
   { dg-final { scan-assembler-times {vmul\.f16\tq[0-9]+, q[0-9]+, q[0-9]+} 2 } }  */
BINOP_LANE_TEST (vmul, 2)
/* { dg-final { scan-assembler-times {vmul\.f16\td[0-9]+, d[0-9]+, d[0-9]+\[2\]} 1 } }
   { dg-final { scan-assembler-times {vmul\.f16\tq[0-9]+, q[0-9]+, d[0-9]+\[2\]} 1 } }  */
BINOP_N_TEST (vmul)
/* { dg-final { scan-assembler-times {vmul\.f16\td[0-9]+, d[0-9]+, d[0-9]+} 3 } }
   { dg-final { scan-assembler-times {vmul\.f16\tq[0-9]+, q[0-9]+, q[0-9]+} 2 } }*/

float16x4_t
test_vpadd_16x4 (float16x4_t a, float16x4_t b)
{
  return vpadd_f16 (a, b);
}
/* { dg-final { scan-assembler-times {vpadd\.f16\td[0-9]+, d[0-9]+, d[0-9]+} 1 } } */

float16x4_t
test_vpmax_16x4 (float16x4_t a, float16x4_t b)
{
  return vpmax_f16 (a, b);
}
/* { dg-final { scan-assembler-times {vpmax\.f16\td[0-9]+, d[0-9]+, d[0-9]+} 1 } } */

float16x4_t
test_vpmin_16x4 (float16x4_t a, float16x4_t b)
{
  return vpmin_f16 (a, b);
}
/* { dg-final { scan-assembler-times {vpmin\.f16\td[0-9]+, d[0-9]+, d[0-9]+} 1 } } */

BINOP_TEST (vsub)
/* { dg-final { scan-assembler-times {vsub\.f16\td[0-9]+, d[0-9]+, d[0-9]+} 1 } }
   { dg-final { scan-assembler-times {vsub\.f16\tq[0-9]+, q[0-9]+, q[0-9]+} 1 } }  */

BINOP_TEST (vrecps)
/* { dg-final { scan-assembler-times {vrecps\.f16\td[0-9]+, d[0-9]+, d[0-9]+} 1 } }
  { dg-final { scan-assembler-times {vrecps\.f16\tq[0-9]+, q[0-9]+, q[0-9]+} 1 } }  */

BINOP_TEST (vrsqrts)
/* { dg-final { scan-assembler-times {vrsqrts\.f16\td[0-9]+, d[0-9]+, d[0-9]+} 1 } }
  { dg-final { scan-assembler-times {vrsqrts\.f16\tq[0-9]+, q[0-9]+, q[0-9]+} 1 } }  */

TERNOP_TEST (vfma)
/* { dg-final { scan-assembler-times {vfma\.f16\td[0-9]+, d[0-9]+, d[0-9]+} 1 } }
  { dg-final { scan-assembler-times {vfma\.f16\tq[0-9]+, q[0-9]+, q[0-9]+} 1 } }  */

TERNOP_TEST (vfms)
/* { dg-final { scan-assembler-times {vfms\.f16\td[0-9]+, d[0-9]+, d[0-9]+} 1 } }
  { dg-final { scan-assembler-times {vfms\.f16\tq[0-9]+, q[0-9]+, q[0-9]+} 1 } }  */

float16x4_t
test_vmov_n_f16 (float16_t a)
{
  return vmov_n_f16 (a);
}

float16x4_t
test_vdup_n_f16 (float16_t a)
{
  return vdup_n_f16 (a);
}
/* { dg-final { scan-assembler-times {vdup\.16\td[0-9]+, r[0-9]+} 3 } }  */

float16x8_t
test_vmovq_n_f16 (float16_t a)
{
  return vmovq_n_f16 (a);
}

float16x8_t
test_vdupq_n_f16 (float16_t a)
{
  return vdupq_n_f16 (a);
}
/* { dg-final { scan-assembler-times {vdup\.16\tq[0-9]+, r[0-9]+} 3 } }  */

float16x4_t
test_vdup_lane_f16 (float16x4_t a)
{
  return vdup_lane_f16 (a, 1);
}
/* { dg-final { scan-assembler-times {vdup\.16\td[0-9]+, d[0-9]+\[1\]} 1 } }  */

float16x8_t
test_vdupq_lane_f16 (float16x4_t a)
{
  return vdupq_lane_f16 (a, 1);
}
/* { dg-final { scan-assembler-times {vdup\.16\tq[0-9]+, d[0-9]+\[1\]} 1 } }  */

float16x4_t
test_vext_f16 (float16x4_t a, float16x4_t b)
{
  return vext_f16 (a, b, 1);
}
/* { dg-final { scan-assembler-times {vext\.16\td[0-9]+, d[0-9]+, d[0-9]+, #1} 1 } } */

float16x8_t
test_vextq_f16 (float16x8_t a, float16x8_t b)
{
  return vextq_f16 (a, b, 1);
}
/*   { dg-final { scan-assembler-times {vext\.16\tq[0-9]+, q[0-9]+, q[0-9]+, #1} 1 } }  */

UNOP_TEST (vrev64)
/* { dg-final { scan-assembler-times {vrev64\.16\td[0-9]+, d[0-9]+} 1 } }
   { dg-final { scan-assembler-times {vrev64\.16\tq[0-9]+, q[0-9]+} 1 } }  */

float16x4_t
test_vbsl16x4 (uint16x4_t a, float16x4_t b, float16x4_t c)
{
  return vbsl_f16 (a, b, c);
}
/* { dg-final { scan-assembler-times {vbsl\td[0-9]+, d[0-9]+, d[0-9]+} 1 } }  */

float16x8_t
test_vbslq16x8 (uint16x8_t a, float16x8_t b, float16x8_t c)
{
  return vbslq_f16 (a, b, c);
}
/*{ dg-final { scan-assembler-times {vbsl\tq[0-9]+, q[0-9]+, q[0-9]+} 1 } }  */

float16x4x2_t
test_vzip16x4 (float16x4_t a, float16x4_t b)
{
  return vzip_f16 (a, b);
}
/* { dg-final { scan-assembler-times {vzip\.16\td[0-9]+, d[0-9]+} 1 } }  */

float16x8x2_t
test_vzipq16x8 (float16x8_t a, float16x8_t b)
{
  return vzipq_f16 (a, b);
}
/*{ dg-final { scan-assembler-times {vzip\.16\tq[0-9]+, q[0-9]+} 1 } }  */

float16x4x2_t
test_vuzp16x4 (float16x4_t a, float16x4_t b)
{
  return vuzp_f16 (a, b);
}
/* { dg-final { scan-assembler-times {vuzp\.16\td[0-9]+, d[0-9]+} 1 } }  */

float16x8x2_t
test_vuzpq16x8 (float16x8_t a, float16x8_t b)
{
  return vuzpq_f16 (a, b);
}
/*{ dg-final { scan-assembler-times {vuzp\.16\tq[0-9]+, q[0-9]+} 1 } }  */

float16x4x2_t
test_vtrn16x4 (float16x4_t a, float16x4_t b)
{
  return vtrn_f16 (a, b);
}
/* { dg-final { scan-assembler-times {vtrn\.16\td[0-9]+, d[0-9]+} 1 } }  */

float16x8x2_t
test_vtrnq16x8 (float16x8_t a, float16x8_t b)
{
  return vtrnq_f16 (a, b);
}
/*{ dg-final { scan-assembler-times {vtrn\.16\tq[0-9]+, q[0-9]+} 1 } }  */
