#ifndef TEST_SVE_ACLE_H
#define TEST_SVE_ACLE_H 1

#include <arm_neon_sve_bridge.h>

#if defined (TEST_OVERLOADS)
#define INVOKE(CODE1, CODE2) CODE2
#elif defined (TEST_FULL)
#define INVOKE(CODE1, CODE2) CODE1
#else
#error "Please define -DTEST_OVERLOADS or -DTEST_FULL"
#endif

#ifdef STREAMING_COMPATIBLE
#define SM_ATTR __arm_streaming_compatible
#elif defined(STREAMING)
#define SM_ATTR __arm_streaming
#else
#define SM_ATTR
#endif

#ifdef SHARED_ZA
#define ZA_ATTR __arm_inout("za")
#else
#define ZA_ATTR
#endif

#ifdef SHARED_ZT0
#define ZT0_ATTR __arm_inout("zt0")
#else
#define ZT0_ATTR
#endif

#define ATTR SM_ATTR ZA_ATTR ZT0_ATTR

#ifdef __cplusplus
#define PROTO(NAME, RET, ARGS) \
  extern "C" RET NAME ARGS ATTR; RET NAME ARGS ATTR
#else
#define PROTO(NAME, RET, ARGS) RET NAME ARGS ATTR
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

#define TEST_DUAL_P(NAME, TYPE1, TYPE2, CODE1, CODE2)		\
  PROTO (NAME, TYPE1, (TYPE1 p0, TYPE1 p1, TYPE2 p2, TYPE2 p3))	\
  {								\
    INVOKE (CODE1, CODE2);					\
    return p0;							\
  }

#define TEST_DUAL_P_REV(NAME, TYPE1, TYPE2, CODE1, CODE2)	\
  PROTO (NAME, TYPE1, (TYPE2 p0, TYPE2 p1, TYPE1 p2, TYPE1 p3))	\
  {								\
    TYPE1 p0_res;						\
    INVOKE (CODE1, CODE2);					\
    return p0_res;						\
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

#define TEST_LOAD_COUNT(NAME, TTYPE, STYPE, CODE1, CODE2) \
  PROTO (NAME, void, (const STYPE *x0, intptr_t x1))	\
  {							\
    register svcount_t pn0 __asm ("pn0");		\
    register svcount_t pn7 __asm ("pn7");		\
    register svcount_t pn8 __asm ("pn8");		\
    register svcount_t pn15 __asm ("pn15");		\
    register TTYPE z0 __asm ("z0");			\
    register TTYPE z17 __asm ("z17");			\
    register TTYPE z22 __asm ("z22");			\
    register TTYPE z28 __asm ("z28");			\
    __asm volatile ("" : "=Upa" (pn0), "=Upa" (pn7),	\
		    "=Upa" (pn8), "=Upa" (pn15));	\
    INVOKE (CODE1, CODE2);				\
    __asm volatile ("" :: "w" (z0), "w" (z17),		\
		    "w" (z22), "w" (z28));		\
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

#define TEST_STORE_COUNT(NAME, TTYPE, STYPE, CODE1, CODE2) \
  PROTO (NAME, void, (STYPE *x0, intptr_t x1))		\
  {							\
    register svcount_t pn0 __asm ("pn0");		\
    register svcount_t pn7 __asm ("pn7");		\
    register svcount_t pn8 __asm ("pn8");		\
    register svcount_t pn15 __asm ("pn15");		\
    register TTYPE z0 __asm ("z0");			\
    register TTYPE z17 __asm ("z17");			\
    register TTYPE z22 __asm ("z22");			\
    register TTYPE z28 __asm ("z28");			\
    __asm volatile ("" : "=Upa" (pn0), "=Upa" (pn7),	\
		    "=Upa" (pn8), "=Upa" (pn15),	\
		    "=w" (z0), "=w" (z17), "=w" (z22),	\
		    "=w" (z28));			\
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

#define TEST_PN(NAME, CODE1, CODE2)			\
  PROTO (NAME, void, (void))				\
  {							\
    register svcount_t pn0 __asm("pn0");		\
    register svcount_t pn7 __asm("pn7");		\
    register svcount_t pn8 __asm("pn8");		\
    register svcount_t pn15 __asm("pn15");		\
    INVOKE (CODE1, CODE2);				\
    __asm volatile ("" :: "Upa" (pn0), "Upa" (pn7),	\
		    "Upa" (pn8), "Upa" (pn15));		\
  }

#define TEST_COUNT_PN(NAME, CODE1, CODE2) 		\
  PROTO (NAME, void, (void))				\
  {							\
    register svcount_t pn0 __asm ("pn0");		\
    register svcount_t pn7 __asm ("pn7");		\
    register svcount_t pn8 __asm ("pn8");		\
    register svcount_t pn15 __asm ("pn15");		\
    register uint64_t x0 __asm ("x0");			\
    register uint64_t x15 __asm ("x15");		\
    register uint64_t x17 __asm ("x17");		\
    __asm volatile ("" : "=Upa" (pn0), "=Upa" (pn7),	\
		    "=Upa" (pn8), "=Upa" (pn15));	\
    INVOKE (CODE1, CODE2);				\
    __asm volatile ("" :: "r" (x0), "r" (x15),		\
		    "r" (x17));				\
  }

#define TEST_EXTRACT_PN(NAME, TYPE, CODE1, CODE2) 	\
  PROTO (NAME, void, (void))				\
  {							\
    register svcount_t pn0 __asm ("pn0");		\
    register TYPE p2 __asm ("p2");			\
    register TYPE p5 __asm ("p5");			\
    register svcount_t pn7 __asm ("pn7");		\
    register svcount_t pn8 __asm ("pn8");		\
    register TYPE p9 __asm ("p9");			\
    register svcount_t pn11 __asm ("pn11");		\
    register TYPE p12 __asm ("p12");			\
    register svcount_t pn15 __asm ("pn15");		\
    __asm volatile ("" : "=Upa" (pn0), "=Upa" (pn7),	\
		    "=Upa" (pn8), "=Upa" (pn11),	\
		    "=Upa" (pn15));			\
    INVOKE (CODE1, CODE2);				\
    __asm volatile ("" :: "Upa" (p2), "Upa" (p5),	\
		    "Upa" (p9), "Upa" (p12));		\
  }

#define TEST_SELECT_P(NAME, TYPE, CODE1, CODE2) 	\
  PROTO (NAME, void, (void))				\
  {							\
    register TYPE p0 __asm ("p0");			\
    register TYPE p2 __asm ("p2");			\
    register svbool_t p7 __asm ("p7");			\
    register svbool_t p8 __asm ("p8");			\
    register TYPE p13 __asm ("p13");			\
    register svbool_t p15 __asm ("p15");		\
    register int32_t w11 __asm ("w11");			\
    register int32_t w12 __asm ("w12");			\
    register int32_t w15 __asm ("w15");			\
    register int32_t w16 __asm ("w16");			\
    __asm volatile ("" : "=Upa" (p0), "=Upa" (p2),	\
		    "=Upa" (p7), "=Upa" (p8),		\
		    "=Upa" (p13), "=Upa" (p15),		\
		    "=r" (w11), "=r" (w12),		\
		    "=r" (w15), "=r" (w16));		\
    INVOKE (CODE1, CODE2);				\
    __asm volatile ("" :: "Upa" (p0), "Upa" (p2),	\
		    "Upa" (p7), "Upa" (p8),		\
		    "Upa" (p13), "Upa" (p15));		\
  }

#define TEST_COMPARE_S(NAME, TYPE, CODE1, CODE2)	\
  PROTO (NAME, svbool_t, (TYPE x0, TYPE x1))		\
  {							\
    svbool_t p0;					\
    INVOKE (CODE1, CODE2);				\
    return p0;						\
  }

#define TEST_COMPARE_S_X2(NAME, TYPE, CODE1, CODE2)	\
  PROTO (NAME, void, (TYPE x0, TYPE x1))		\
  {							\
    register svboolx2_t p1 __asm("p1");			\
    register svboolx2_t p4 __asm("p4");			\
    register svboolx2_t p9 __asm("p9");			\
    register svboolx2_t p14 __asm("p14");		\
    INVOKE (CODE1, CODE2);				\
    __asm volatile ("" :: "Upa" (p1), "Upa" (p4),	\
		    "Upa" (p9), "Upa" (p14));		\
  }

#define TEST_COMPARE_S_C(NAME, TYPE, CODE1, CODE2)	\
  PROTO (NAME, void, (TYPE x0, TYPE x1))		\
  {							\
    register svcount_t pn0 __asm("pn0");		\
    register svcount_t pn7 __asm("pn7");		\
    register svcount_t pn8 __asm("pn8");		\
    register svcount_t pn15 __asm("pn15");		\
    INVOKE (CODE1, CODE2);				\
    __asm volatile ("" :: "Upa" (pn0), "Upa" (pn7),	\
		    "Upa" (pn8), "Upa" (pn15));		\
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

#define TEST_CREATE_B(NAME, TTYPE, CODE1, CODE2)		\
  PROTO (NAME, TTYPE, (svbool_t p0, svbool_t p1,		\
		       svbool_t p2, svbool_t p3))		\
  {								\
    TTYPE p0_res;						\
    INVOKE (CODE1, CODE2);					\
    return p0_res;						\
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

#define TEST_GET_B(NAME, TTYPE, CODE1, CODE2)			\
  PROTO (NAME, void, (void))					\
  {								\
    register svbool_t p0 __asm ("p0");				\
    register TTYPE p4 __asm ("p4");				\
    register svbool_t p4_res __asm ("p4");			\
    register svbool_t p5_res __asm ("p5");			\
    register svbool_t p6_res __asm ("p6");			\
    register svbool_t p7_res __asm ("p7");			\
    __asm volatile ("" : "=Upa" (p0), "=Upa" (p4));		\
    INVOKE (CODE1, CODE2);					\
    __asm volatile ("" :: "Upa" (p0), "Upa" (p4_res),		\
		    "Upa" (p5_res), "Upa" (p6_res),		\
		    "Upa" (p7_res));				\
  }

#define TEST_SET(NAME, TTYPE, ZTYPE, CODE1, CODE2)		\
  PROTO (NAME, void, (ZTYPE z0, ZTYPE z1, ZTYPE z2, ZTYPE z3,	\
		      TTYPE z4))				\
  {								\
    register TTYPE z24 __asm ("z24");				\
    INVOKE (CODE1, CODE2);					\
    __asm volatile ("" :: "w" (z4), "w" (z24));			\
  }

#define TEST_SET_B(NAME, TTYPE, CODE1, CODE2)			\
  PROTO (NAME, void, (void))					\
  {								\
    register svbool_t p0 __asm ("p0");				\
    register TTYPE p4 __asm ("p4");				\
    register TTYPE p8 __asm ("p8");				\
    register svbool_t p12 __asm ("p12");			\
    register svbool_t p13 __asm ("p13");			\
    __asm volatile ("" : "=Upa" (p0), "=Upa" (p4),		\
		    "=Upa" (p12), "=Upa" (p13));		\
    INVOKE (CODE1, CODE2);					\
    __asm volatile ("" :: "Upa" (p4), "Upa" (p8));		\
  }

#define TEST_SET_NEONQ(NAME, TTYPE, ZTYPE, CODE1, CODE2)	\
  PROTO (NAME, void, (ZTYPE z0, ZTYPE z1, ZTYPE z2, ZTYPE z3,	\
		      TTYPE z4))				\
  {								\
    register TTYPE z24 __asm ("z24");				\
    register TTYPE z4_res __asm ("z4");				\
    INVOKE (CODE1, CODE2);					\
    __asm volatile ("" :: "w" (z24), "w" (z4_res));	\
  }

#define TEST_DUP_NEONQ(NAME, TTYPE, ZTYPE, CODE1, CODE2)	\
  PROTO (NAME, void, (ZTYPE unused0, ZTYPE unused1,		\
		      ZTYPE unused2, ZTYPE unused3, TTYPE z4))	\
  {								\
    register ZTYPE z0 __asm ("z0");				\
    register ZTYPE z4_res __asm ("z4");				\
    register ZTYPE z5_res __asm ("z5");				\
    INVOKE (CODE1, CODE2);					\
    __asm volatile ("" :: "w" (z0), "w" (z4_res),		\
		    "w" (z5_res));				\
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

#define TEST_XN(NAME, TTYPE, RES, CODE1, CODE2)			\
  PROTO (NAME, void, ())					\
  {								\
    register TTYPE z0 __asm ("z0");				\
    register TTYPE z4 __asm ("z4");				\
    register TTYPE z18 __asm ("z18");				\
    register TTYPE z23 __asm ("z23");				\
    register TTYPE z28 __asm ("z28");				\
    register svcount_t pn0 __asm ("pn0");			\
    register svcount_t pn7 __asm ("pn7");			\
    register svcount_t pn8 __asm ("pn8");			\
    register svcount_t pn15 __asm ("pn15");			\
    __asm volatile ("" : "=w" (z0), "=w" (z4), "=w" (z18),	\
		    "=w" (z23), "=w" (z28), "=Upa" (pn0),	\
		    "=Upa" (pn7), "=Upa" (pn8), "=Upa" (pn15));	\
    INVOKE (RES = CODE1, RES = CODE2);				\
    __asm volatile ("" :: "w" (RES));				\
  }

#define TEST_DUAL_XN(NAME, TTYPE1, TTYPE2, RES, CODE1, CODE2)	\
  PROTO (NAME, void, ())					\
  {								\
    register TTYPE1 z0 __asm ("z0");				\
    register TTYPE2 z4 __asm ("z4");				\
    register TTYPE1 z18 __asm ("z18");				\
    register TTYPE2 z23 __asm ("z23");				\
    register TTYPE1 z28 __asm ("z28");				\
    __asm volatile ("" : "=w" (z0), "=w" (z4), "=w" (z18),	\
		    "=w" (z23), "=w" (z28));			\
    INVOKE (RES = CODE1, RES = CODE2);				\
    __asm volatile ("" :: "w" (RES));				\
  }

#define TEST_XN_SINGLE(NAME, TTYPE, ZTYPE, RES, CODE1, CODE2)	\
  PROTO (NAME, void, ())					\
  {								\
    register ZTYPE z0 __asm ("z0");				\
    register TTYPE z1 __asm ("z1");				\
    register ZTYPE z5 __asm ("z5");				\
    register ZTYPE z7 __asm ("z7");				\
    register ZTYPE z16 __asm ("z16");				\
    register TTYPE z18 __asm ("z18");				\
    register ZTYPE z23 __asm ("z23");				\
    register TTYPE z24 __asm ("z24");				\
    register TTYPE z28 __asm ("z28");				\
    __asm volatile ("" : "=w" (z0), "=w" (z1), "=w" (z5),	\
		    "=w" (z7), "=w" (z16), "=w" (z18),		\
		    "=w" (z23), "=w" (z24), "=w" (z28));	\
    INVOKE (RES = CODE1, RES = CODE2);				\
    __asm volatile ("" :: "w" (RES));				\
  }

#define TEST_XN_SINGLE_Z15(NAME, TTYPE, ZTYPE, CODE1, CODE2)	\
  PROTO (NAME, TTYPE, (TTYPE z0))				\
  {								\
    register ZTYPE z15 __asm ("z15");				\
    __asm volatile ("" : "=w" (z15));				\
    INVOKE (CODE1, CODE2);					\
    return z0;							\
  }

#define TEST_XN_SINGLE_AWKWARD(NAME, TTYPE, ZTYPE, CODE1, CODE2) \
  PROTO (NAME, TTYPE, (ZTYPE z0, TTYPE z1, ZTYPE zn))		\
  {								\
    TTYPE z0_res;						\
    INVOKE (CODE1, CODE2);					\
    return z0_res;						\
  }

#define TEST_X2_NARROW(NAME, TTYPE, ZTYPE, CODE1, CODE2)	\
  PROTO (NAME, void, ())					\
  {								\
    register TTYPE z0 __asm ("z0");				\
    register ZTYPE z5 __asm ("z5");				\
    register TTYPE z6 __asm ("z6");				\
    register TTYPE z16 __asm ("z16");				\
    register ZTYPE z22 __asm ("z22");				\
    register TTYPE z29 __asm ("z29");				\
    register ZTYPE z0_res __asm ("z0");				\
    __asm volatile ("" : "=w" (z0), "=w" (z5), "=w" (z6),	\
		    "=w" (z16), "=w" (z22), "=w" (z29));	\
    INVOKE (CODE1, CODE2);					\
    __asm volatile ("" :: "w" (z0_res), "w" (z5), "w" (z22));	\
  }

#define TEST_X4_NARROW(NAME, TTYPE, ZTYPE, CODE1, CODE2)	\
  PROTO (NAME, void, ())					\
  {								\
    register TTYPE z0 __asm ("z0");				\
    register TTYPE z4 __asm ("z4");				\
    register TTYPE z16 __asm ("z16");				\
    register TTYPE z21 __asm ("z21");				\
    register ZTYPE z25 __asm ("z25");				\
    register TTYPE z26 __asm ("z26");				\
    register ZTYPE z0_res __asm ("z0");				\
    register ZTYPE z22_res __asm ("z22");			\
    __asm volatile ("" : "=w" (z0), "=w" (z4), "=w" (z16),	\
		    "=w" (z21), "=w" (z26));			\
    INVOKE (CODE1, CODE2);					\
    __asm volatile ("" :: "w" (z0_res), "w" (z22_res),		\
		    "w" (z25));					\
  }

#endif
