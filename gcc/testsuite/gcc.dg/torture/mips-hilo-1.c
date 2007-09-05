/* f1 checks that an mtlo is not moved before an mfhi.  f2 does the same
   for an mthi and an mflo.  */
/* { dg-do run { target mips*-*-* } } */
/* { dg-options "-mtune=rm7000" } */

extern void abort (void);
extern void exit (int);

#define DECLARE(TYPE)							\
  TYPE __attribute__ ((noinline)) __attribute__ ((nomips16))		\
  f1##TYPE (TYPE x1, TYPE x2, TYPE x3)					\
  {									\
    TYPE t1, t2;							\
									\
    asm ("mult\t%1,%2" : "=h" (t1) : "d" (x1), "d" (x2) : "lo");	\
    asm ("mflo\t%0" : "=r" (t2) : "l" (x3) : "hi");			\
    return t1 + t2;							\
  }									\
									\
  TYPE __attribute__ ((noinline)) __attribute__ ((nomips16))		\
  f2##TYPE (TYPE x1, TYPE x2, TYPE x3)					\
  {									\
    TYPE t1, t2;							\
									\
    asm ("mult\t%1,%2" : "=l" (t1) : "d" (x1), "d" (x2) : "hi");	\
    asm ("mfhi\t%0" : "=r" (t2) : "h" (x3) : "lo");			\
    return t1 + t2;							\
  }

#define TEST(TYPE)							\
  if (f1##TYPE (1, 2, 10) != 10)					\
    abort ();								\
  if (f2##TYPE (1, 2, 40) != 42)					\
    abort ()

typedef char c;
typedef signed char sc;
typedef unsigned char uc;
typedef short s;
typedef unsigned short us;
typedef int i;
typedef unsigned int ui;
typedef long long ll;
typedef unsigned long long ull;

DECLARE (c)
DECLARE (sc)
DECLARE (uc)
DECLARE (s)
DECLARE (us)
DECLARE (i)
DECLARE (ui)
#if defined (__mips64)
DECLARE (ll)
DECLARE (ull)
#endif

int
main ()
{
  TEST (c);
  TEST (sc);
  TEST (uc);
  TEST (s);
  TEST (us);
  TEST (i);
  TEST (ui);
#if defined (__mips64)
  TEST (ll);
  TEST (ull);
#endif
  exit (0);
}
