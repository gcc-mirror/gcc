#include <stdarg.h>
#include "compat-common.h"
#include "mixed-struct-defs.h"
#include "mixed-struct-check.h"

#ifdef SKIP_VA
const int test_va = 0;
#else
const int test_va = 1;
#endif

#ifndef SKIP_VLA_IN_STRUCT
#define T(NAME, FIELDS, TYPE, FIELDINIT, FIELDTEST)			\
void									\
testva##NAME (int n, ...)						\
{									\
  va_list ap;								\
  if (test_va)								\
    {									\
      struct S { FIELDS TYPE a[n]; } s;					\
      int fail = 0, i, j;						\
									\
      va_start (ap, n);							\
      for (j = 0; j < 2; ++j)						\
        {								\
	  s = va_arg (ap, struct S);					\
	  for (i = 0; i < n; ++i)					\
	    if (s.a[i] != 12 + n - i)					\
	      ++fail;							\
	  if (fail)							\
	    { DEBUG_FAIL; }						\
	  if (!j && va_arg (ap, int) != n)				\
	    { DEBUG_FAIL; }						\
	  FIELDTEST;							\
	}								\
      va_end (ap);							\
    }									\
}

#include "struct-by-value-22_x.c"
#endif
