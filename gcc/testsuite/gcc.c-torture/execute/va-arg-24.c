/* The purpose of this code is to test argument passing of a tuple of
   11 integers, with the break point between named and unnamed arguments
   at every possible position.	*/

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

static int errors = 0;

static void
verify (const char *tcase, int n[11])
{
  int i;
  for (i = 0; i <= 10; i++)
    if (n[i] != i)
      {
	printf (" %s: n[%d] = %d expected %d\n", tcase, i, n[i], i);
	errors++;
      }
}

#define STR(x) #x

#define p(i) int q##i,
#define P(i) n[i] = q##i;

#define p0 p(0)
#define p1 p(1)
#define p2 p(2)
#define p3 p(3)
#define p4 p(4)
#define p5 p(5)
#define p6 p(6)
#define p7 p(7)
#define p8 p(8)
#define p9 p(9)

#define P0 P(0)
#define P1 P(1)
#define P2 P(2)
#define P3 P(3)
#define P4 P(4)
#define P5 P(5)
#define P6 P(6)
#define P7 P(7)
#define P8 P(8)
#define P9 P(9)

#define TCASE(x, params, vecinit)		\
static void					\
varargs##x (params ...)				\
{						\
  va_list ap;					\
  int n[11];					\
  int i;					\
						\
  va_start (ap, q##x);				\
  vecinit					\
  for (i = x + 1; i <= 10; i++)			\
    n[i] = va_arg (ap, int);			\
  va_end (ap);					\
						\
  verify (STR(varargs##x), n);			\
}

#define TEST(x) varargs##x (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

TCASE(0, p0			      , P0			     )
TCASE(1, p0 p1			      , P0 P1			     )
TCASE(2, p0 p1 p2		      , P0 P1 P2		     )
TCASE(3, p0 p1 p2 p3		      , P0 P1 P2 P3		     )
TCASE(4, p0 p1 p2 p3 p4		      , P0 P1 P2 P3 P4		     )
TCASE(5, p0 p1 p2 p3 p4 p5	      , P0 P1 P2 P3 P4 P5	     )
TCASE(6, p0 p1 p2 p3 p4 p5 p6	      , P0 P1 P2 P3 P4 P5 P6	     )
TCASE(7, p0 p1 p2 p3 p4 p5 p6 p7      , P0 P1 P2 P3 P4 P5 P6 P7	     )
TCASE(8, p0 p1 p2 p3 p4 p5 p6 p7 p8   , P0 P1 P2 P3 P4 P5 P6 P7 P8   )
TCASE(9, p0 p1 p2 p3 p4 p5 p6 p7 p8 p9, P0 P1 P2 P3 P4 P5 P6 P7 P8 P9)

int main(void)
{
  TEST(0);
  TEST(1);
  TEST(2);
  TEST(3);
  TEST(4);
  TEST(5);
  TEST(6);
  TEST(7);
  TEST(8);
  TEST(9);

  if (errors)
    abort ();
  exit (0);
}
