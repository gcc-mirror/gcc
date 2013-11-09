/* Test for _Atomic in C11.  Basic execution tests for atomic compound
   assignment.  */
/* { dg-do run } */
/* { dg-options "-std=c11 -pedantic-errors" } */

extern void abort (void);
extern void exit (int);

#define CMPLX(X, Y) __builtin_complex ((X), (Y))

#define TEST_COMPOUND(TYPE, LHSVAL, RHSVAL, OP)				\
  do									\
    {									\
      static volatile _Atomic (TYPE) a = (TYPE) (LHSVAL);		\
      if ((a OP##= (RHSVAL)) != (TYPE) ((TYPE) (LHSVAL) OP (RHSVAL)))	\
	abort ();							\
      if (a != (TYPE) ((TYPE) (LHSVAL) OP (RHSVAL)))			\
	abort ();							\
    }									\
  while (0)

#define TEST_COMPOUND_ARITH(LHSVAL, RHSVAL, OP)				\
  do									\
    {									\
      TEST_COMPOUND (_Bool, (LHSVAL), (RHSVAL), OP);			\
      TEST_COMPOUND (char, (LHSVAL), (RHSVAL), OP);			\
      TEST_COMPOUND (signed char, (LHSVAL), (RHSVAL), OP);		\
      TEST_COMPOUND (unsigned char, (LHSVAL), (RHSVAL), OP);		\
      TEST_COMPOUND (signed short, (LHSVAL), (RHSVAL), OP);		\
      TEST_COMPOUND (unsigned short, (LHSVAL), (RHSVAL), OP);		\
      TEST_COMPOUND (signed int, (LHSVAL), (RHSVAL), OP);		\
      TEST_COMPOUND (unsigned int, (LHSVAL), (RHSVAL), OP);		\
      TEST_COMPOUND (signed long, (LHSVAL), (RHSVAL), OP);		\
      TEST_COMPOUND (unsigned long, (LHSVAL), (RHSVAL), OP);		\
      TEST_COMPOUND (signed long long, (LHSVAL), (RHSVAL), OP);		\
      TEST_COMPOUND (unsigned long long, (LHSVAL), (RHSVAL), OP);	\
      TEST_COMPOUND (float, (LHSVAL), (RHSVAL), OP);			\
      TEST_COMPOUND (double, (LHSVAL), (RHSVAL), OP);			\
      TEST_COMPOUND (long double, (LHSVAL), (RHSVAL), OP);		\
      TEST_COMPOUND (_Complex float, (LHSVAL), (RHSVAL), OP);		\
      TEST_COMPOUND (_Complex double, (LHSVAL), (RHSVAL), OP);		\
      TEST_COMPOUND (_Complex long double, (LHSVAL), (RHSVAL), OP);	\
    }									\
  while (0)

#define TEST_COMPOUND_INT(LHSVAL, RHSVAL, OP)				\
  do									\
    {									\
      TEST_COMPOUND (_Bool, (LHSVAL), (RHSVAL), OP);			\
      TEST_COMPOUND (char, (LHSVAL), (RHSVAL), OP);			\
      TEST_COMPOUND (signed char, (LHSVAL), (RHSVAL), OP);		\
      TEST_COMPOUND (unsigned char, (LHSVAL), (RHSVAL), OP);		\
      TEST_COMPOUND (signed short, (LHSVAL), (RHSVAL), OP);		\
      TEST_COMPOUND (unsigned short, (LHSVAL), (RHSVAL), OP);		\
      TEST_COMPOUND (signed int, (LHSVAL), (RHSVAL), OP);		\
      TEST_COMPOUND (unsigned int, (LHSVAL), (RHSVAL), OP);		\
      TEST_COMPOUND (signed long, (LHSVAL), (RHSVAL), OP);		\
      TEST_COMPOUND (unsigned long, (LHSVAL), (RHSVAL), OP);		\
      TEST_COMPOUND (signed long long, (LHSVAL), (RHSVAL), OP);		\
      TEST_COMPOUND (unsigned long long, (LHSVAL), (RHSVAL), OP);	\
    }									\
  while (0)

static void
test_mult (void)
{
  TEST_COMPOUND_ARITH (1, 2, *);
  TEST_COMPOUND_ARITH (-3, 5, *);
  TEST_COMPOUND_ARITH (-7, -20, *);
  TEST_COMPOUND_ARITH (1.25, 3.5, *);
  TEST_COMPOUND_ARITH (CMPLX (1.5, 2.5), CMPLX (3.5, 4.5), *);
  TEST_COMPOUND_ARITH (CMPLX (1.5, 2.5), 2, *);
}

static void
test_div (void)
{
  TEST_COMPOUND_ARITH (1, 2, /);
  TEST_COMPOUND_ARITH (-6, 3, /);
  TEST_COMPOUND_ARITH (-70, -10, /);
  TEST_COMPOUND_ARITH (1.25, 2.5, /);
  TEST_COMPOUND_ARITH (CMPLX (1.0, 1.0), CMPLX (0.5, 0.5), /);
  TEST_COMPOUND_ARITH (CMPLX (1.5, 2.5), 2, /);
}

static void
test_mod (void)
{
  TEST_COMPOUND_INT (1, 2, %);
  TEST_COMPOUND_INT (-3, 5, %);
  TEST_COMPOUND_INT (-7, -2, %);
}

static void
test_plus (void)
{
  TEST_COMPOUND_ARITH (1, 2, +);
  TEST_COMPOUND_ARITH (-3, 5, +);
  TEST_COMPOUND_ARITH (-7, -20, +);
  TEST_COMPOUND_ARITH (1.25, 3.5, +);
  TEST_COMPOUND_ARITH (CMPLX (1.5, 2.5), CMPLX (3.5, 4.5), +);
  TEST_COMPOUND_ARITH (CMPLX (1.5, 2.5), 2, +);
  static int ia[2];
  TEST_COMPOUND (int *, &ia[1], 1, +);
  TEST_COMPOUND (int *, &ia[1], -1, +);
}

static void
test_minus (void)
{
  TEST_COMPOUND_ARITH (1, 2, -);
  TEST_COMPOUND_ARITH (-3, 5, -);
  TEST_COMPOUND_ARITH (-7, -20, -);
  TEST_COMPOUND_ARITH (3.5, 1.25, -);
  TEST_COMPOUND_ARITH (CMPLX (3.5, 4.5), CMPLX (1.5, 2.5), -);
  TEST_COMPOUND_ARITH (CMPLX (3.5, 2.5), 2, -);
  static int ia[2];
  TEST_COMPOUND (int *, &ia[1], 1, -);
  TEST_COMPOUND (int *, &ia[1], -1, -);
}

static void
test_lshift (void)
{
  TEST_COMPOUND_INT (1, 7, <<);
  TEST_COMPOUND_INT (15, 3, <<);
}

static void
test_rshift (void)
{
  TEST_COMPOUND_INT (1, 1, >>);
  TEST_COMPOUND_INT (127, 4, >>);
}

static void
test_and (void)
{
  TEST_COMPOUND_INT (0x1234, 0x7856, &);
  TEST_COMPOUND_INT (-1, 0x12345678, &);
}

static void
test_xor (void)
{
  TEST_COMPOUND_INT (0x1234, 0x7856, ^);
  TEST_COMPOUND_INT (-1, 0x12345678, ^);
}

static void
test_or (void)
{
  TEST_COMPOUND_INT (0x1234, 0x7856, |);
  TEST_COMPOUND_INT (-12345, 0x12345678, |);
}

int
main (void)
{
  test_mult ();
  test_div ();
  test_mod ();
  test_plus ();
  test_minus ();
  test_lshift ();
  test_rshift ();
  test_and ();
  test_xor ();
  test_or ();
  exit (0);
}
