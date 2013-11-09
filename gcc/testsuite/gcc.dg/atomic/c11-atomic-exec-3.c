/* Test for _Atomic in C11.  Basic execution tests for atomic
   increment and decrement.  */
/* { dg-do run } */
/* { dg-options "-std=c11 -pedantic-errors" } */

extern void abort (void);
extern void exit (int);

#define TEST_INCDEC(TYPE, VALUE, PREOP, POSTOP, PRE_P, CHANGE)		\
  do									\
    {									\
      static volatile _Atomic (TYPE) a = (TYPE) (VALUE);		\
      if (PREOP a POSTOP != (PRE_P					\
			     ? (TYPE) ((TYPE) (VALUE) + (CHANGE))	\
			     : (TYPE) (VALUE)))				\
	abort ();							\
      if (a != (TYPE) ((TYPE) (VALUE) + (CHANGE)))			\
	abort ();							\
    }									\
  while (0)

#define TEST_INCDEC_ARITH(VALUE, PREOP, POSTOP, PRE_P, CHANGE)		\
  do									\
    {									\
      TEST_INCDEC (_Bool, (VALUE), PREOP, POSTOP, (PRE_P), (CHANGE));	\
      TEST_INCDEC (char, (VALUE), PREOP, POSTOP, (PRE_P), (CHANGE));	\
      TEST_INCDEC (signed char, (VALUE), PREOP, POSTOP, (PRE_P),	\
		   (CHANGE));						\
      TEST_INCDEC (unsigned char, (VALUE), PREOP, POSTOP, (PRE_P),	\
		   (CHANGE));						\
      TEST_INCDEC (signed short, (VALUE), PREOP, POSTOP, (PRE_P),	\
		   (CHANGE));						\
      TEST_INCDEC (unsigned short, (VALUE), PREOP, POSTOP, (PRE_P),	\
		   (CHANGE));						\
      TEST_INCDEC (signed int, (VALUE), PREOP, POSTOP, (PRE_P),		\
		   (CHANGE));						\
      TEST_INCDEC (unsigned int, (VALUE), PREOP, POSTOP, (PRE_P),	\
		   (CHANGE));						\
      TEST_INCDEC (signed long, (VALUE), PREOP, POSTOP, (PRE_P),	\
		   (CHANGE));						\
      TEST_INCDEC (unsigned long, (VALUE), PREOP, POSTOP, (PRE_P),	\
		   (CHANGE));						\
      TEST_INCDEC (signed long long, (VALUE), PREOP, POSTOP, (PRE_P),	\
		   (CHANGE));						\
      TEST_INCDEC (unsigned long long, (VALUE), PREOP, POSTOP, (PRE_P), \
		   (CHANGE));						\
      TEST_INCDEC (float, (VALUE), PREOP, POSTOP, (PRE_P), (CHANGE));	\
      TEST_INCDEC (double, (VALUE), PREOP, POSTOP, (PRE_P), (CHANGE));	\
      TEST_INCDEC (long double, (VALUE), PREOP, POSTOP, (PRE_P),	\
		   (CHANGE));						\
    }									\
  while (0)

#define TEST_ALL_INCDEC_ARITH(VALUE)		\
  do						\
    {						\
      TEST_INCDEC_ARITH ((VALUE), ++, , 1, 1);	\
      TEST_INCDEC_ARITH ((VALUE), --, , 1, -1);	\
      TEST_INCDEC_ARITH ((VALUE), , ++, 0, 1);	\
      TEST_INCDEC_ARITH ((VALUE), , --, 0, -1);	\
    }						\
  while (0)

static void
test_incdec (void)
{
  TEST_ALL_INCDEC_ARITH (0);
  TEST_ALL_INCDEC_ARITH (1);
  TEST_ALL_INCDEC_ARITH (2);
  TEST_ALL_INCDEC_ARITH (-1);
  TEST_ALL_INCDEC_ARITH (1ULL << 60);
  TEST_ALL_INCDEC_ARITH (1.5);
  static int ia[2];
  TEST_INCDEC (int *, &ia[1], ++, , 1, 1);
  TEST_INCDEC (int *, &ia[1], --, , 1, -1);
  TEST_INCDEC (int *, &ia[1], , ++, 0, 1);
  TEST_INCDEC (int *, &ia[1], , --, 0, -1);
}

int
main (void)
{
  test_incdec ();
  exit (0);
}
