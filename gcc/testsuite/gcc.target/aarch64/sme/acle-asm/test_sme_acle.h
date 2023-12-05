#ifndef TEST_SME_ACLE_H
#define TEST_SME_ACLE_H 1

#if (!defined(STREAMING_COMPATIBLE) \
     && !defined(NON_STREAMING) \
     && !defined(STREAMING))
#define STREAMING
#endif

#if !defined(NO_SHARED_ZA)
#define SHARED_ZA
#endif

#include "../../sve/acle/asm/test_sve_acle.h"

#include <arm_sme.h>

#define TEST_LOAD_ZA(NAME, CODE1, CODE2)			\
  PROTO (NAME, void, (svbool_t p0, int32_t w0, const char *x1,	\
		      uint64_t x2))				\
  {								\
    INVOKE (CODE1, CODE2);					\
  }

#define TEST_STORE_ZA(NAME, CODE1, CODE2)			\
  PROTO (NAME, void, (svbool_t p0, int32_t w0, char *x1,	\
		      uint64_t x2))				\
  {								\
    INVOKE (CODE1, CODE2);					\
  }

#define TEST_READ_ZA(NAME, TYPE, CODE1, CODE2)			\
  PROTO (NAME, TYPE, (TYPE z0, TYPE z1, svbool_t p0,		\
		      int32_t w0))				\
  {								\
    INVOKE (CODE1, CODE2);					\
    return z0;							\
  }

#define TEST_WRITE_ZA(NAME, TYPE, CODE1, CODE2)			\
  PROTO (NAME, void, (TYPE z0, TYPE z1, svbool_t p0,		\
		      int32_t w0))				\
  {								\
    INVOKE (CODE1, CODE2);					\
  }

#define TEST_UNIFORM_ZA(NAME, TYPE, CODE1, CODE2)		\
  PROTO (NAME, void, (TYPE z0, TYPE z1, svbool_t p0,		\
		      svbool_t p1))				\
  {								\
    INVOKE (CODE1, CODE2);					\
  }

#define TEST_DUAL_ZA(NAME, TYPE1, TYPE2, CODE1, CODE2)		\
  PROTO (NAME, void, (TYPE1 z0, TYPE1 z1, TYPE1 z2, TYPE1 z3,	\
		      TYPE2 z4, TYPE2 z5, TYPE2 z6, TYPE2 z7,	\
		      svbool_t p0, svbool_t p1))		\
  {								\
    INVOKE (CODE1, CODE2);					\
  }

#endif
