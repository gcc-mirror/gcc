#ifndef TEST_SME2_ACLE_H
#define TEST_SME2_ACLE_H 1

#include "../../sme/acle-asm/test_sme_acle.h"

#define TEST_ZA_X1(NAME, ZTYPE, CODE1, CODE2)			\
  PROTO (NAME, void, (int w0))					\
  {								\
    register int w7 __asm ("w7");				\
    register int w8 __asm ("w8");				\
    register int w9 __asm ("w9");				\
    register int w10 __asm ("w10");				\
    register int w11 __asm ("w11");				\
    register int w12 __asm ("w12");				\
    register ZTYPE z0 __asm ("z0");				\
    register ZTYPE z3 __asm ("z3");				\
    register ZTYPE z7 __asm ("z7");				\
    register ZTYPE z16 __asm ("z16");				\
    register ZTYPE z23 __asm ("z23");				\
    register ZTYPE z31 __asm ("z31");				\
    __asm volatile ("" : "=r" (w7), "=r" (w8), "=r" (w9),	\
		    "=r" (w10), "=r" (w11), "=r" (w12),		\
		    "=w" (z0), "=w" (z3), "=w" (z7),		\
		    "=w" (z16), "=w" (z23), "=w" (z31));	\
    INVOKE (CODE1, CODE2);					\
  }

#define TEST_ZA_XN(NAME, TTYPE, CODE1, CODE2)			\
  PROTO (NAME, void, (int w0))					\
  {								\
    register int w7 __asm ("w7");				\
    register int w8 __asm ("w8");				\
    register int w11 __asm ("w11");				\
    register int w12 __asm ("w12");				\
    register int w15 __asm ("w15");				\
    register int w16 __asm ("w16");				\
    register TTYPE z0 __asm ("z0");				\
    register TTYPE z4 __asm ("z4");				\
    register TTYPE z18 __asm ("z18");				\
    register TTYPE z23 __asm ("z23");				\
    register TTYPE z28 __asm ("z28");				\
    __asm volatile ("" : "=r" (w7), "=r" (w8), "=r" (w11),	\
		    "=r" (w12), "=r" (w15), "=r" (w16),		\
		    "=w" (z0), "=w" (z4), "=w" (z18),		\
		    "=w" (z23), "=w" (z28));			\
    INVOKE (CODE1, CODE2);					\
  }

#define TEST_READ_ZA_XN(NAME, TTYPE, CODE1, CODE2)		\
  PROTO (NAME, void, (int w0))					\
  {								\
    register int w7 __asm ("w7");				\
    register int w8 __asm ("w8");				\
    register int w11 __asm ("w11");				\
    register int w12 __asm ("w12");				\
    register int w15 __asm ("w15");				\
    register int w16 __asm ("w16");				\
    register TTYPE z0 __asm ("z0");				\
    register TTYPE z4 __asm ("z4");				\
    register TTYPE z18 __asm ("z18");				\
    register TTYPE z23 __asm ("z23");				\
    register TTYPE z28 __asm ("z28");				\
    __asm volatile ("" : "=r" (w7), "=r" (w8), "=r" (w11),	\
		    "=r" (w12), "=r" (w15), "=r" (w16));	\
    INVOKE (CODE1, CODE2);					\
    __asm volatile ("" :: "w" (z0), "w" (z4), "w" (z18),	\
		    "w" (z23), "w" (z28));			\
  }

#define TEST_ZA_SINGLE(NAME, TTYPE, ZTYPE, CODE1, CODE2)	\
  PROTO (NAME, void, (int w0))					\
  {								\
    register int w8 __asm ("w8");				\
    register int w11 __asm ("w11");				\
    register ZTYPE z0 __asm ("z0");				\
    register TTYPE z1 __asm ("z1");				\
    register ZTYPE z16 __asm ("z16");				\
    register TTYPE z20 __asm ("z20");				\
    register TTYPE z27 __asm ("z27");				\
    __asm volatile ("" : "=r" (w8), "=r" (w11), "=w" (z0),	\
		    "=w" (z1), "=w" (z16), "=w" (z20),		\
		    "=w" (z27));				\
    INVOKE (CODE1, CODE2);					\
  }

#define TEST_ZA_SINGLE_Z15(NAME, TTYPE, ZTYPE, CODE1, CODE2)	\
  PROTO (NAME, void, (int w0))					\
  {								\
    register int w8 __asm ("w8");				\
    register TTYPE z0 __asm ("z0");				\
    register ZTYPE z15 __asm ("z15");				\
    __asm volatile ("" : "=r" (w8), "=w" (z0), "=w" (z15));	\
    INVOKE (CODE1, CODE2);					\
  }

#define TEST_ZA_LANE(NAME, TTYPE, ZTYPE, CODE1, CODE2)		\
  PROTO (NAME, void, (int w0))					\
  {								\
    register int w8 __asm ("w8");				\
    register int w11 __asm ("w11");				\
    register TTYPE z0 __asm ("z0");				\
    register ZTYPE z4 __asm ("z4");				\
    register ZTYPE z7 __asm ("z7");				\
    register ZTYPE z16 __asm ("z16");				\
    register TTYPE z17 __asm ("z17");				\
    register TTYPE z22 __asm ("z22");				\
    register TTYPE z28 __asm ("z28");				\
    __asm volatile ("" : "=r" (w8), "=r" (w11), "=w" (z0),	\
		    "=w" (z4), "=w" (z7), "=w" (z16),		\
		    "=w" (z17), "=w" (z22), "=w" (z28));	\
    INVOKE (CODE1, CODE2);					\
  }

#define TEST_ZA_LANE_Z15(NAME, TTYPE, ZTYPE, CODE1, CODE2)	\
  PROTO (NAME, void, (int w0))					\
  {								\
    register int w8 __asm ("w8");				\
    register TTYPE z4 __asm ("z4");				\
    register ZTYPE z15 __asm ("z15");				\
    __asm volatile ("" : "=r" (w8), "=w" (z4), "=w" (z15));	\
    INVOKE (CODE1, CODE2);					\
  }

#endif
