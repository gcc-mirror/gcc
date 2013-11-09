/* Test for _Atomic in C11.  Test of valid assignment cases for
   arithmetic types.  */
/* { dg-do compile } */
/* { dg-options "-std=c11 -pedantic-errors" } */

#define TEST_ASSIGN(TYPE1, OP, TYPE2)		\
  do						\
    {						\
      _Atomic TYPE1 a = 0;			\
      TYPE2 b = 0;				\
      _Atomic TYPE2 c = 0;			\
      a OP b;					\
      a OP c;					\
    }						\
  while (0)

#define TEST_ASSIGN_ARITHR(TYPE1, OP)			\
  do							\
    {							\
      TEST_ASSIGN (TYPE1, OP, _Bool);			\
      TEST_ASSIGN (TYPE1, OP, char);			\
      TEST_ASSIGN (TYPE1, OP, signed char);		\
      TEST_ASSIGN (TYPE1, OP, unsigned char);		\
      TEST_ASSIGN (TYPE1, OP, signed short);		\
      TEST_ASSIGN (TYPE1, OP, unsigned short);		\
      TEST_ASSIGN (TYPE1, OP, signed int);		\
      TEST_ASSIGN (TYPE1, OP, unsigned int);		\
      TEST_ASSIGN (TYPE1, OP, signed long);		\
      TEST_ASSIGN (TYPE1, OP, unsigned long);		\
      TEST_ASSIGN (TYPE1, OP, signed long long);	\
      TEST_ASSIGN (TYPE1, OP, unsigned long long);	\
      TEST_ASSIGN (TYPE1, OP, float);			\
      TEST_ASSIGN (TYPE1, OP, double);			\
      TEST_ASSIGN (TYPE1, OP, long double);		\
      TEST_ASSIGN (TYPE1, OP, _Complex float);		\
      TEST_ASSIGN (TYPE1, OP, _Complex double);		\
      TEST_ASSIGN (TYPE1, OP, _Complex long double);	\
    }							\
  while (0)

#define TEST_ASSIGN_ARITHBOTH(OP)			\
  do							\
    {							\
      TEST_ASSIGN_ARITHR (_Bool, OP);			\
      TEST_ASSIGN_ARITHR (char, OP);			\
      TEST_ASSIGN_ARITHR (signed char, OP);		\
      TEST_ASSIGN_ARITHR (unsigned char, OP);		\
      TEST_ASSIGN_ARITHR (signed short, OP);		\
      TEST_ASSIGN_ARITHR (unsigned short, OP);		\
      TEST_ASSIGN_ARITHR (signed int, OP);		\
      TEST_ASSIGN_ARITHR (unsigned int, OP);		\
      TEST_ASSIGN_ARITHR (signed long, OP);		\
      TEST_ASSIGN_ARITHR (unsigned long, OP);		\
      TEST_ASSIGN_ARITHR (signed long long, OP);	\
      TEST_ASSIGN_ARITHR (unsigned long long, OP);	\
      TEST_ASSIGN_ARITHR (float, OP);			\
      TEST_ASSIGN_ARITHR (double, OP);			\
      TEST_ASSIGN_ARITHR (long double, OP);		\
      TEST_ASSIGN_ARITHR (_Complex float, OP);		\
      TEST_ASSIGN_ARITHR (_Complex double, OP);		\
      TEST_ASSIGN_ARITHR (_Complex long double, OP);	\
    }							\
  while (0)

#define TEST_ASSIGN_INTR(TYPE1, OP)			\
  do							\
    {							\
      TEST_ASSIGN (TYPE1, OP, _Bool);			\
      TEST_ASSIGN (TYPE1, OP, char);			\
      TEST_ASSIGN (TYPE1, OP, signed char);		\
      TEST_ASSIGN (TYPE1, OP, unsigned char);		\
      TEST_ASSIGN (TYPE1, OP, signed short);		\
      TEST_ASSIGN (TYPE1, OP, unsigned short);		\
      TEST_ASSIGN (TYPE1, OP, signed int);		\
      TEST_ASSIGN (TYPE1, OP, unsigned int);		\
      TEST_ASSIGN (TYPE1, OP, signed long);		\
      TEST_ASSIGN (TYPE1, OP, unsigned long);		\
      TEST_ASSIGN (TYPE1, OP, signed long long);	\
      TEST_ASSIGN (TYPE1, OP, unsigned long long);	\
    }							\
  while (0)

#define TEST_ASSIGN_INTBOTH(OP)				\
  do							\
    {							\
      TEST_ASSIGN_INTR (_Bool, OP);			\
      TEST_ASSIGN_INTR (char, OP);			\
      TEST_ASSIGN_INTR (signed char, OP);		\
      TEST_ASSIGN_INTR (unsigned char, OP);		\
      TEST_ASSIGN_INTR (signed short, OP);		\
      TEST_ASSIGN_INTR (unsigned short, OP);		\
      TEST_ASSIGN_INTR (signed int, OP);		\
      TEST_ASSIGN_INTR (unsigned int, OP);		\
      TEST_ASSIGN_INTR (signed long, OP);		\
      TEST_ASSIGN_INTR (unsigned long, OP);		\
      TEST_ASSIGN_INTR (signed long long, OP);		\
      TEST_ASSIGN_INTR (unsigned long long, OP);	\
    }							\
  while (0)

void
test_simple (void)
{
  TEST_ASSIGN_ARITHBOTH (=);
}

void
test_mult (void)
{
  TEST_ASSIGN_ARITHBOTH (*=);
}

void
test_div (void)
{
  TEST_ASSIGN_ARITHBOTH (/=);
}

void
test_mod (void)
{
  TEST_ASSIGN_INTBOTH (%=);
}

void
test_plus (void)
{
  TEST_ASSIGN_ARITHBOTH (+=);
}

void
test_minus (void)
{
  TEST_ASSIGN_ARITHBOTH (-=);
}

void
test_lshift (void)
{
  TEST_ASSIGN_INTBOTH (<<=);
}

void
test_rshift (void)
{
  TEST_ASSIGN_INTBOTH (>>=);
}

void
test_and (void)
{
  TEST_ASSIGN_INTBOTH (&=);
}

void
test_xor (void)
{
  TEST_ASSIGN_INTBOTH (^=);
}

void
test_or (void)
{
  TEST_ASSIGN_INTBOTH (|=);
}
