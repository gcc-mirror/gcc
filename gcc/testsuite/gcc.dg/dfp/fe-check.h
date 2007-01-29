/* Common support for checking that appropriate floating point exceptions
   are raised for decimal float operations.  These tests are here to test
   the software decimal float support in libgcc.  */

#include "dfp-except.h"

extern void abort (void);
static int failcnt = 0;

/* Support compiling the test to report individual failures; default is
   to abort as soon as a check fails.  */
#if defined(DBG) || defined(DBG2)
#include <stdio.h>
#define FAILURE(NUM,KIND,EXCEPT) \
  { printf ("failed for test %d: %s %s\n", NUM, KIND, EXCEPT); failcnt++; }
#else
#define FAILURE(N,K,E) abort ();
#endif

/* This is useful when modifying the test to make sure that tests are
   actually run.  */
#if defined(DBG2)
#define SUCCESS(NUM,EXCEPT) \
  { printf ("passed for test %d: %s\n", NUM, EXCEPT); }
#else
#define SUCCESS(N,E) ;
#endif

#define CHECKFLAG(NUM,EXCEPT,GOT,WANT)				\
  if ((WANT & EXCEPT) != (GOT & EXCEPT))			\
    {								\
      if ((WANT & EXCEPT) != 0)					\
        FAILURE (NUM, "missing", #EXCEPT)			\
      else							\
        FAILURE (NUM, "unexpected", #EXCEPT)			\
    }								\
  else								\
    SUCCESS (NUM, #EXCEPT)

void
checkflags (int num, int want)
{
  int got = DFP_TEST_EXCEPT (FE_ALL_EXCEPT);
  CHECKFLAG (num, FE_INVALID, got, want)
  CHECKFLAG (num, FE_OVERFLOW, got, want)
  CHECKFLAG (num, FE_UNDERFLOW, got, want)
  CHECKFLAG (num, FE_DIVBYZERO, got, want)
  CHECKFLAG (num, FE_INEXACT, got, want)
}

#define BINOP(NUM,OP,VAR1,VAL1,VAR2,VAL2,VAR3,EXCEPT)		\
void								\
binop_##NUM (void)						\
{								\
  VAR1 = VAL1;							\
  VAR2 = VAL2;							\
  DFP_CLEAR_EXCEPT (FE_ALL_EXCEPT);				\
  VAR3 = VAR1 OP VAR2;						\
  checkflags (NUM, EXCEPT);					\
}

#define CONVERT(NUM,FROM,TO,VALUE,EXCEPT)			\
void								\
convert_##NUM (void)						\
{								\
  FROM = VALUE;							\
  DFP_CLEAR_EXCEPT (FE_ALL_EXCEPT);				\
  TO = FROM;							\
  checkflags (NUM, EXCEPT);					\
}
