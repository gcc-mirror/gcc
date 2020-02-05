#ifndef TEST_SVE_ACLE_H
#define TEST_SVE_ACLE_H 1

#include <arm_sve.h>

#if defined (TEST_OVERLOADS)
#define INVOKE(CODE1, CODE2) CODE2
#elif defined (TEST_FULL)
#define INVOKE(CODE1, CODE2) CODE1
#else
#error "Please define -DTEST_OVERLOADS or -DTEST_FULL"
#endif

#ifdef __cplusplus
#define PROTO(NAME, RET, ARGS) extern "C" RET NAME ARGS; RET NAME ARGS
#else
#define PROTO(NAME, RET, ARGS) RET NAME ARGS
#endif

#define TEST_UNIFORM_Z(NAME, TYPE, CODE1, CODE2)		\
  PROTO (NAME, TYPE, (TYPE z0, TYPE z1, TYPE z2, TYPE z3,	\
		      svbool_t p0, svbool_t p1))		\
  {								\
    INVOKE (CODE1, CODE2);					\
    return z0;							\
  }

#define TEST_UNIFORM_P(NAME, CODE1, CODE2)		\
  PROTO (NAME, svbool_t, (svbool_t p0, svbool_t p1,	\
			  svbool_t p2, svbool_t p3))	\
  {							\
    INVOKE (CODE1, CODE2);				\
    return p0;						\
  }

#define TEST_UNIFORM_P_SINGLE(NAME, CODE)		\
  PROTO (NAME, svbool_t, (svbool_t p0, svbool_t p1,	\
			  svbool_t p2, svbool_t p3))	\
  {							\
    CODE;						\
    return p0;						\
  }

#define TEST_UNIFORM_S(NAME, TYPE, CODE1, CODE2)		\
  PROTO (NAME, TYPE, (TYPE x0, TYPE x1, TYPE x2, TYPE x3,	\
		      svbool_t p0, svbool_t p1))		\
  {								\
    INVOKE (CODE1, CODE2);					\
    return x0;							\
  }

#define TEST_DUAL_Z(NAME, TYPE1, TYPE2, CODE1, CODE2)		\
  PROTO (NAME, TYPE1, (TYPE1 z0, TYPE1 z1, TYPE1 z2, TYPE1 z3,	\
		       TYPE2 z4, TYPE2 z5, TYPE2 z6, TYPE2 z7,	\
		       svbool_t p0, svbool_t p1))		\
  {								\
    INVOKE (CODE1, CODE2);					\
    return z0;							\
  }

#define TEST_DUAL_Z_REV(NAME, TYPE1, TYPE2, CODE1, CODE2)	\
  PROTO (NAME, TYPE1, (TYPE2 z0, TYPE2 z1, TYPE2 z2, TYPE2 z3,	\
		       TYPE1 z4, TYPE1 z5, TYPE1 z6, TYPE1 z7,	\
		       svbool_t p0, svbool_t p1))		\
  {								\
    TYPE1 z0_res;						\
    INVOKE (CODE1, CODE2);					\
    return z0_res;						\
  }

#define TEST_TRIPLE_Z(NAME, TYPE1, TYPE2, TYPE3, CODE1, CODE2)	\
  PROTO (NAME, TYPE1, (TYPE1 z0, TYPE1 z1, TYPE2 z2, TYPE2 z3,	\
		       TYPE3 z4, TYPE3 z5,			\
		       svbool_t p0, svbool_t p1))		\
  {								\
    INVOKE (CODE1, CODE2);					\
    return z0;							\
  }

#define TEST_TRIPLE_Z_REV2(NAME, TYPE1, TYPE2, TYPE3, CODE1, CODE2)\
  PROTO (NAME, TYPE1, (TYPE2 z0, TYPE2 z1, TYPE1 z2, TYPE1 z3,	\
		       TYPE3 z4, TYPE3 z5,			\
		       svbool_t p0, svbool_t p1))		\
  {								\
    TYPE1 z0_res;						\
    INVOKE (CODE1, CODE2);					\
    return z0_res;						\
  }

#define TEST_TRIPLE_Z_REV(NAME, TYPE1, TYPE2, TYPE3, CODE1, CODE2)\
  PROTO (NAME, TYPE1, (TYPE3 z0, TYPE3 z1, TYPE2 z2, TYPE2 z3,	\
		       TYPE1 z4, TYPE1 z5,			\
		       svbool_t p0, svbool_t p1))		\
  {								\
    TYPE1 z0_res;						\
    INVOKE (CODE1, CODE2);					\
    return z0_res;						\
  }

#define TEST_DUAL_LANE_REG(NAME, ZTYPE1, ZTYPE2, REG, CODE1, CODE2) \
  PROTO (NAME, void, (void))					\
  {								\
    register ZTYPE1 z0 __asm ("z0");				\
    register ZTYPE2 z1 __asm ("z1");				\
    register ZTYPE2 REG __asm (#REG);				\
    __asm volatile ("" : "=w" (z0), "=w" (z1), "=w" (REG));	\
    INVOKE (CODE1, CODE2);					\
    __asm volatile ("" :: "w" (z0));				\
  }

#define TEST_TRIPLE_LANE_REG(NAME, ZTYPE1, ZTYPE2, ZTYPE3, REG, CODE1, CODE2) \
  PROTO (NAME, void, (void))					\
  {								\
    register ZTYPE1 z0 __asm ("z0");				\
    register ZTYPE2 z1 __asm ("z1");				\
    register ZTYPE3 REG __asm (#REG);				\
    __asm volatile ("" : "=w" (z0), "=w" (z1), "=w" (REG));	\
    INVOKE (CODE1, CODE2);					\
    __asm volatile ("" :: "w" (z0));				\
  }

#define TEST_TYPE_CHANGE_Z(NAME, TYPE1, TYPE2, CODE1, CODE2)	\
  PROTO (NAME, TYPE1, (TYPE2 z0, TYPE2 z1, TYPE2 z2, TYPE2 z3,	\
		       svbool_t p0, svbool_t p1))		\
  {								\
    TYPE1 z0_res;						\
    INVOKE (CODE1, CODE2);					\
    return z0_res;						\
  }

#define TEST_UNIFORM_ZX(NAME, ZTYPE, STYPE, CODE1, CODE2)	\
  PROTO (NAME, ZTYPE, (ZTYPE z0, ZTYPE z1, ZTYPE z2, ZTYPE z3,	\
		       svbool_t p0, STYPE x0))			\
  {								\
    INVOKE (CODE1, CODE2);					\
    return z0;							\
  }

#define TEST_UNIFORM_ZD(NAME, ZTYPE, STYPE, CODE1, CODE2)	\
  PROTO (NAME, ZTYPE, (ZTYPE z0, ZTYPE z1, ZTYPE z2, ZTYPE z3,	\
		       svbool_t p0, STYPE d4))			\
  {								\
    INVOKE (CODE1, CODE2);					\
    return z0;							\
  }

#define TEST_UNIFORM_PS(NAME, CODE1, CODE2)			\
  PROTO (NAME, svbool_t, (svbool_t p0, svbool_t p1,		\
			  svbool_t p2, svbool_t p3, bool x0))	\
  {								\
    INVOKE (CODE1, CODE2);					\
    return p0;							\
  }

#define TEST_DUAL_ZD(NAME, ZTYPE1, ZTYPE2, STYPE, CODE1, CODE2)	\
  PROTO (NAME, ZTYPE1, (ZTYPE1 z0, ZTYPE1 z1, ZTYPE1 z2,	\
			ZTYPE1 z3, ZTYPE2 z4, ZTYPE2 z5,	\
			ZTYPE2 z6, STYPE d7, svbool_t p0,	\
			svbool_t p1))				\
  {								\
    INVOKE (CODE1, CODE2);					\
    return z0;							\
  }

#define TEST_DUAL_ZX(NAME, ZTYPE1, ZTYPE2, STYPE, CODE1, CODE2)	\
  PROTO (NAME, ZTYPE1, (ZTYPE1 z0, ZTYPE1 z1, ZTYPE1 z2,	\
			ZTYPE1 z3, ZTYPE2 z4, ZTYPE2 z5,	\
			ZTYPE2 z6, ZTYPE2 z7, svbool_t p0,	\
			svbool_t p1, STYPE x0))			\
  {								\
    INVOKE (CODE1, CODE2);					\
    return z0;							\
  }

#define TEST_TRIPLE_ZX(NAME, TYPE1, TYPE2, TYPE3, CODE1, CODE2)	\
  PROTO (NAME, TYPE1, (TYPE1 z0, TYPE1 z1, TYPE2 z2, TYPE2 z3,	\
		       TYPE3 x0, TYPE3 x1,			\
		       svbool_t p0, svbool_t p1))		\
  {								\
    INVOKE (CODE1, CODE2);					\
    return z0;							\
  }

#define TEST_TYPE_CHANGE_ZX(NAME, ZTYPE1, ZTYPE2, STYPE, CODE1, CODE2) \
  PROTO (NAME, ZTYPE1, (ZTYPE2 z0, ZTYPE2 z1, ZTYPE2 z2,	\
			ZTYPE2 z3, svbool_t p0, svbool_t p1,	\
			STYPE x0))				\
  {								\
    ZTYPE1 z0_res;						\
    INVOKE (CODE1, CODE2);					\
    return z0_res;						\
  }

#define TEST_LOAD(NAME, ZTYPE, STYPE, CODE1, CODE2)	\
  PROTO (NAME, ZTYPE, (svbool_t p0, const STYPE *x0,	\
		       intptr_t x1))			\
  {							\
    ZTYPE z0;						\
    INVOKE (CODE1, CODE2);				\
    return z0;						\
  }

#define TEST_LOAD_GATHER_SZ(NAME, RES_TYPE, STYPE, ZTYPE, CODE1, CODE2) \
  PROTO (NAME, RES_TYPE, (ZTYPE z0, ZTYPE z1, svbool_t p0,	\
			  const STYPE *x0))			\
  {								\
    RES_TYPE z0_res;						\
    INVOKE (CODE1, CODE2);					\
    return z0_res;						\
  }

#define TEST_LOAD_GATHER_ZS(NAME, RES_TYPE, ZTYPE, CODE1, CODE2) \
  PROTO (NAME, RES_TYPE, (ZTYPE z0, ZTYPE z1, svbool_t p0,	\
			  int64_t x0))				\
  {								\
    RES_TYPE z0_res;						\
    INVOKE (CODE1, CODE2);					\
    return z0_res;						\
  }

#define TEST_PREFETCH(NAME, STYPE, CODE1, CODE2)	\
  PROTO (NAME, void, (svbool_t p0, const STYPE *x0,	\
		      intptr_t x1))			\
  {							\
    INVOKE (CODE1, CODE2);				\
  }

#define TEST_PREFETCH_GATHER_SZ(NAME, ZTYPE, CODE1, CODE2)	\
  PROTO (NAME, void, (ZTYPE z0, ZTYPE z1, svbool_t p0,		\
		      const void *x0))				\
  {								\
    INVOKE (CODE1, CODE2);					\
  }

#define TEST_PREFETCH_GATHER_ZS(NAME, ZTYPE, CODE1, CODE2)	\
  PROTO (NAME, void, (ZTYPE z0, ZTYPE z1, svbool_t p0,		\
		      int64_t x0))				\
  {								\
    INVOKE (CODE1, CODE2);					\
  }

#define TEST_STORE(NAME, ZTYPE, STYPE, CODE1, CODE2)	\
  PROTO (NAME, void, (ZTYPE z0, svbool_t p0, STYPE *x0,	\
		      intptr_t x1))			\
  {							\
    INVOKE (CODE1, CODE2);				\
  }

#define TEST_STORE_SCATTER_SZ(NAME, DATA_TYPE, STYPE, ZTYPE, CODE1, CODE2) \
  PROTO (NAME, void, (DATA_TYPE z0, ZTYPE z1, svbool_t p0,	\
		      STYPE *x0))				\
  {								\
    INVOKE (CODE1, CODE2);					\
  }

#define TEST_STORE_SCATTER_ZS(NAME, DATA_TYPE, ZTYPE, CODE1, CODE2) \
  PROTO (NAME, void, (DATA_TYPE z0, ZTYPE z1, svbool_t p0,	\
		      int64_t x0))				\
  {								\
    INVOKE (CODE1, CODE2);					\
  }

#define TEST_P(NAME, CODE1, CODE2)	\
  PROTO (NAME, svbool_t, (void))	\
  {					\
    svbool_t p0;			\
    INVOKE (CODE1, CODE2);		\
    return p0;				\
  }

#define TEST_PTEST(NAME, TYPE, CODE)				\
  PROTO (NAME, TYPE, (svbool_t p0, svbool_t p1, svbool_t p2,	\
		      svbool_t p3, TYPE x0, TYPE x1))		\
  {								\
    INVOKE (CODE, CODE);					\
    return x0;							\
  }

#define TEST_COMPARE_S(NAME, TYPE, CODE1, CODE2)	\
  PROTO (NAME, svbool_t, (TYPE x0, TYPE x1))		\
  {							\
    svbool_t p0;					\
    INVOKE (CODE1, CODE2);				\
    return p0;						\
  }

#define TEST_COMPARE_Z(NAME, TYPE, CODE1, CODE2)		\
  PROTO (NAME, svbool_t, (TYPE z0, TYPE z1,			\
			  svbool_t p0, svbool_t p1))		\
  {								\
    INVOKE (CODE1, CODE2);					\
    return p0;							\
  }

#define TEST_COMPARE_ZX(NAME, ZTYPE, STYPE, CODE1, CODE2)	\
  PROTO (NAME, svbool_t, (ZTYPE z0, ZTYPE z1, svbool_t p0,	\
			  svbool_t p1, STYPE x0))		\
  {								\
    INVOKE (CODE1, CODE2);					\
    return p0;							\
  }

#define TEST_COMPARE_ZD(NAME, ZTYPE, STYPE, CODE1, CODE2)	\
  PROTO (NAME, svbool_t, (ZTYPE z0, ZTYPE z1, ZTYPE z2,		\
			  ZTYPE z3, svbool_t p0, svbool_t p1,	\
			  STYPE d4))				\
  {								\
    INVOKE (CODE1, CODE2);					\
    return p0;							\
  }

#define TEST_COMPARE_DUAL_Z(NAME, TYPE1, TYPE2, CODE1, CODE2) \
  PROTO (NAME, svbool_t, (TYPE1 z0, TYPE2 z1,		\
			  svbool_t p0, svbool_t p1))	\
  {							\
    INVOKE (CODE1, CODE2);				\
    return p0;						\
  }

#define TEST_REDUCTION_X(NAME, STYPE, ZTYPE, CODE1, CODE2) \
  PROTO (NAME, STYPE, (ZTYPE z0, ZTYPE z1, svbool_t p0))   \
  {							   \
    STYPE x0;						   \
    INVOKE (CODE1, CODE2);				   \
    return x0;						   \
  }

#define TEST_REDUCTION_D(NAME, STYPE, ZTYPE, CODE1, CODE2) \
  PROTO (NAME, STYPE, (ZTYPE z0, ZTYPE z1, svbool_t p0))   \
  {							   \
    STYPE d0;						   \
    INVOKE (CODE1, CODE2);				   \
    return d0;						   \
  }

#define TEST_FOLD_LEFT_D(NAME, STYPE, ZTYPE, CODE1, CODE2) \
  PROTO (NAME, STYPE, (STYPE d0, STYPE d1, ZTYPE z2,	\
		       svbool_t p0))   			\
  {							\
    INVOKE (CODE1, CODE2);				\
    return d0;						\
  }

#define TEST_FOLD_LEFT_X(NAME, STYPE, ZTYPE, CODE1, CODE2) \
  PROTO (NAME, STYPE, (STYPE x0, STYPE x1, ZTYPE z0,	\
		       svbool_t p0))   			\
  {							\
    INVOKE (CODE1, CODE2);				\
    return x0;						\
  }

#define TEST_S(NAME, ZTYPE, STYPE, CODE)	\
  PROTO (NAME, ZTYPE, (STYPE x0, STYPE x1))	\
  {						\
    ZTYPE z0;					\
    CODE;					\
    return z0;					\
  }

#define TEST_ADR(NAME, TYPE1, TYPE2, CODE1, CODE2)	\
  PROTO (NAME, TYPE1, (TYPE1 z0, TYPE2 z1))		\
  {							\
    INVOKE (CODE1, CODE2);				\
    return z0;						\
  }

#define TEST_UNDEF(NAME, TYPE, CODE)	\
  PROTO (NAME, TYPE, (void))		\
  {					\
    TYPE z0;				\
    CODE;				\
    return z0;				\
  }

#define TEST_CREATE(NAME, TTYPE, ZTYPE, CODE1, CODE2)		\
  PROTO (NAME, TTYPE, (ZTYPE unused0, ZTYPE unused1,		\
		       ZTYPE unused2, ZTYPE unused3,		\
		       ZTYPE z4, ZTYPE z5, ZTYPE z6, ZTYPE z7))	\
  {								\
    TTYPE z0;							\
    INVOKE (CODE1, CODE2);					\
    return z0;							\
  }

#define TEST_GET(NAME, TTYPE, ZTYPE, CODE1, CODE2)		\
  PROTO (NAME, void, (ZTYPE unused0, ZTYPE unused1,		\
		      ZTYPE unused2, ZTYPE unused3, TTYPE z4))	\
  {								\
    register ZTYPE z0 __asm ("z0");				\
    register ZTYPE z4_res __asm ("z4");				\
    register ZTYPE z5_res __asm ("z5");				\
    register ZTYPE z6_res __asm ("z6");				\
    register ZTYPE z7_res __asm ("z7");				\
    INVOKE (CODE1, CODE2);					\
    __asm volatile ("" :: "w" (z0), "w" (z4_res), "w" (z5_res),	\
		    "w" (z6_res), "w" (z7_res));		\
  }

#define TEST_SET(NAME, TTYPE, ZTYPE, CODE1, CODE2)		\
  PROTO (NAME, void, (ZTYPE z0, ZTYPE z1, ZTYPE z2, ZTYPE z3,	\
		      TTYPE z4))				\
  {								\
    register TTYPE z24 __asm ("z24");				\
    INVOKE (CODE1, CODE2);					\
    __asm volatile ("" :: "w" (z4), "w" (z24));			\
  }

#define TEST_TBL2(NAME, TTYPE, ZTYPE, UTYPE, CODE1, CODE2)	\
  PROTO (NAME, ZTYPE, (TTYPE z0, TTYPE z2, UTYPE z4))		\
  {								\
    register ZTYPE z0_res __asm ("z0");				\
    INVOKE (CODE1, CODE2);					\
    return z0_res;						\
  }

#define TEST_TBL2_REV(NAME, TTYPE, ZTYPE, UTYPE, CODE1, CODE2)	\
  PROTO (NAME, ZTYPE, (UTYPE z0, TTYPE z1, TTYPE z3))		\
  {								\
    register ZTYPE z0_res __asm ("z0");				\
    INVOKE (CODE1, CODE2);					\
    return z0_res;						\
  }

#endif
