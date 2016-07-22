/* { dg-additional-options "-Wno-shift-overflow" } */
/* { dg-require-effective-target vect_shift } */
/* { dg-require-effective-target vect_int } */
/* Check the standard integer types for left and right shifts to see if the
   compiler replaced a scalar instruction with a vector instruction whether the
   correct value is generated.  */

#ifdef TRACE
#endif

#include <stdarg.h>
#include "tree-vect.h"

#ifndef ALIGN
#define ALIGN __attribute__((__aligned__(__BIGGEST_ALIGNMENT__)))
#endif

#ifndef NOINLINE
#define NOINLINE __attribute__((__noinline__))
#endif

#ifdef TRACE
#define TRACE_FUNC(PREFIX, NAME) printf (#PREFIX #NAME "\n")
#define TRACE_DONE()  printf ("done!\n")
#define TRACE_ABORT(I,E,G)						\
do {									\
  printf ("Element %d, expected 0x%lx, got 0x%lx\n",			\
	  I, (long)(E), (long)(G));					\
  abort ();								\
} while (0)

#else
#define TRACE_FUNC(PREFIX, A)
#define TRACE_DONE()
#define TRACE_ABORT(I,E,G) abort ()
#endif

#define NAME(A,B) A ## B

#define VECT_TESTS(PREFIX, TYPE, N)					\
 /* Restrict the optimizer from optimizing the setup loops.  */		\
volatile TYPE NAME (PREFIX, zero) = 0;					\
									\
TYPE NAME (PREFIX, a)[N] ALIGN;						\
TYPE NAME (PREFIX, b)[N] ALIGN;						\
TYPE NAME (PREFIX, c)[N] ALIGN;						\
TYPE NAME (PREFIX, d)[N] ALIGN;						\
									\
static void NOINLINE							\
NAME (PREFIX, lshift_2) (void)						\
{									\
  int i;								\
									\
  TRACE_FUNC (PREFIX, lshift_2);					\
  for (i = 0; i < N; i++)						\
    NAME (PREFIX, a)[i] = NAME (PREFIX, b)[i] << 2;			\
}									\
									\
static void NOINLINE							\
NAME (PREFIX, lshift_var) (int shift)					\
{									\
  int i;								\
									\
  TRACE_FUNC (PREFIX, lshift_var);					\
  for (i = 0; i < N; i++)						\
    NAME (PREFIX, a)[i] = NAME (PREFIX, b)[i] << shift;			\
}									\
									\
static void NOINLINE							\
NAME (PREFIX, lshift_vect) (void)					\
{									\
  int i;								\
									\
  TRACE_FUNC (PREFIX, lshift_vect);					\
  for (i = 0; i < N; i++)						\
    NAME (PREFIX, a)[i] = NAME (PREFIX, b)[i] << NAME (PREFIX, c)[i];	\
}									\
									\
static void NOINLINE							\
NAME (PREFIX, rshift_2) (void)						\
{									\
  int i;								\
									\
  TRACE_FUNC (PREFIX, rshift_2);					\
  for (i = 0; i < N; i++)						\
    NAME (PREFIX, a)[i] = NAME (PREFIX, b)[i] >> 2;			\
}									\
									\
static void NOINLINE							\
NAME (PREFIX, rshift_var) (int shift)					\
{									\
  int i;								\
									\
  TRACE_FUNC (PREFIX, rshift_var);					\
  for (i = 0; i < N; i++)						\
    NAME (PREFIX, a)[i] = NAME (PREFIX, b)[i] >> shift;			\
}									\
									\
static void NOINLINE							\
NAME (PREFIX, rshift_vect) (void)					\
{									\
  int i;								\
									\
  TRACE_FUNC (PREFIX, rshift_vect);					\
  for (i = 0; i < N; i++)						\
    NAME (PREFIX, a)[i] = NAME (PREFIX, b)[i] >> NAME (PREFIX, c)[i];	\
}									\
									\
static void NOINLINE							\
NAME (PREFIX, check) (void)						\
{									\
  int i;								\
									\
  TRACE_FUNC (PREFIX, check);						\
  for (i = 0; i < N; i++)						\
    if (NAME (PREFIX, a)[i] != NAME (PREFIX, d)[i])			\
      TRACE_ABORT (i, NAME (PREFIX, d)[i], NAME (PREFIX, a)[i]);	\
}									\
									\
static void NOINLINE							\
NAME (PREFIX, tests) (void)						\
{									\
  int i;								\
									\
  TRACE_FUNC (PREFIX, tests);						\
  for (i = 0; i < N; i++)						\
    {									\
      NAME (PREFIX, b)[i] = (i + NAME (PREFIX, zero));			\
      NAME (PREFIX, c)[i] = 2;						\
      NAME (PREFIX, d)[i] = (i + NAME (PREFIX, zero)) << 2;		\
    }									\
									\
  NAME (PREFIX, lshift_2) ();						\
  NAME (PREFIX, check) ();						\
									\
  NAME (PREFIX, lshift_var) (2);					\
  NAME (PREFIX, check) ();						\
									\
  NAME (PREFIX, lshift_vect) ();					\
  NAME (PREFIX, check) ();						\
									\
  for (i = 0; i < N; i++)						\
    {									\
      NAME (PREFIX, b)[i] = ((i + NAME (PREFIX, zero)) << 4)		\
	| (((TYPE)0x80) << ((sizeof (TYPE) * 8) - 8));			\
      NAME (PREFIX, c)[i] = 2;						\
      NAME (PREFIX, d)[i] = (TYPE)((NAME (PREFIX, b)[i]			\
				    + NAME (PREFIX, zero)) >> 2);	\
    }									\
									\
  NAME (PREFIX, rshift_2) ();						\
  NAME (PREFIX, check) ();						\
									\
  NAME (PREFIX, rshift_var) (2);					\
  NAME (PREFIX, check) ();						\
									\
  NAME (PREFIX, rshift_vect) ();					\
  NAME (PREFIX, check) ();						\
}

VECT_TESTS (uc_, unsigned char,  128)
VECT_TESTS (us_, unsigned short, 256)
VECT_TESTS (ui_, unsigned int,   256)
VECT_TESTS (ul_, unsigned long,  256)

VECT_TESTS (sc_, signed char,    128)
VECT_TESTS (ss_, short,          256)
VECT_TESTS (si_, int,            256)
VECT_TESTS (sl_, long,           256)

int main ()
{
  int i;

  check_vect ();

  uc_tests ();
  us_tests ();
  ui_tests ();
  ul_tests ();

  sc_tests ();
  ss_tests ();
  si_tests ();
  sl_tests ();

  TRACE_DONE ();
  return 0;
}

