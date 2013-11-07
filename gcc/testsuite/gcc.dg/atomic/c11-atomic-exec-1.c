/* Test for _Atomic in C11.  Basic execution tests for atomic loads
   and stores.  */
/* { dg-do run } */
/* { dg-options "-std=c11 -pedantic-errors" } */

extern void abort (void);
extern void exit (int);
extern int memcmp (const void *, const void *, __SIZE_TYPE__);

#define CMPLX(X, Y) __builtin_complex ((X), (Y))

#define TEST_SIMPLE_ASSIGN(TYPE, VALUE)				\
  do								\
    {								\
      static volatile _Atomic (TYPE) a, b = (TYPE) (VALUE);	\
      if (a != 0)						\
	abort ();						\
      if (b != ((TYPE) (VALUE)))				\
	abort ();						\
      if ((a = b) != ((TYPE) (VALUE)))				\
	abort ();						\
      if (a != ((TYPE) (VALUE)))				\
	abort ();						\
    }								\
  while (0)

#define TEST_SIMPLE_ASSIGN_ARITH(VALUE)				\
  do								\
    {								\
      TEST_SIMPLE_ASSIGN (_Bool, (VALUE));			\
      TEST_SIMPLE_ASSIGN (char, (VALUE));			\
      TEST_SIMPLE_ASSIGN (signed char, (VALUE));		\
      TEST_SIMPLE_ASSIGN (unsigned char, (VALUE));		\
      TEST_SIMPLE_ASSIGN (signed short, (VALUE));		\
      TEST_SIMPLE_ASSIGN (unsigned short, (VALUE));		\
      TEST_SIMPLE_ASSIGN (signed int, (VALUE));			\
      TEST_SIMPLE_ASSIGN (unsigned int, (VALUE));		\
      TEST_SIMPLE_ASSIGN (signed long, (VALUE));		\
      TEST_SIMPLE_ASSIGN (unsigned long, (VALUE));		\
      TEST_SIMPLE_ASSIGN (signed long long, (VALUE));		\
      TEST_SIMPLE_ASSIGN (unsigned long long, (VALUE));		\
      TEST_SIMPLE_ASSIGN (float, (VALUE));			\
      TEST_SIMPLE_ASSIGN (double, (VALUE));			\
      TEST_SIMPLE_ASSIGN (long double, (VALUE));		\
      TEST_SIMPLE_ASSIGN (_Complex float, (VALUE));		\
      TEST_SIMPLE_ASSIGN (_Complex double, (VALUE));		\
      TEST_SIMPLE_ASSIGN (_Complex long double, (VALUE));	\
    }								\
  while (0)

static void
test_simple_assign (void)
{
  TEST_SIMPLE_ASSIGN_ARITH (0);
  TEST_SIMPLE_ASSIGN_ARITH (1);
  TEST_SIMPLE_ASSIGN_ARITH (2);
  TEST_SIMPLE_ASSIGN_ARITH (-1);
  TEST_SIMPLE_ASSIGN_ARITH (1ULL << 63);
  TEST_SIMPLE_ASSIGN_ARITH (1.5);
  TEST_SIMPLE_ASSIGN_ARITH (CMPLX (2.5, 3.5));
  static int i;
  TEST_SIMPLE_ASSIGN (int *, 0);
  TEST_SIMPLE_ASSIGN (int *, &i);
  struct s { short a[1024]; };
  struct s init, copy;
  _Atomic struct s s1, s2;
  for (int j = 0; j < 1024; j++)
    init.a[j] = j;
  copy = (s1 = init);
  if (memcmp (&init, &copy, sizeof init) != 0)
    abort ();
  copy = (s2 = s1);
  if (memcmp (&init, &copy, sizeof init) != 0)
    abort ();
  copy = s1;
  if (memcmp (&init, &copy, sizeof init) != 0)
    abort ();
  copy = s2;
  if (memcmp (&init, &copy, sizeof init) != 0)
    abort ();
}

int
main (void)
{
  test_simple_assign ();
  exit (0);
}
