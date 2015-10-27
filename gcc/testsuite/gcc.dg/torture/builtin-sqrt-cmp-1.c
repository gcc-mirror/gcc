/* { dg-do link } */
/* { dg-options "-ffast-math" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } { "" } } */

extern double sqrt (double);
extern float sqrtf (float);
extern long double sqrtl (long double);

/* All references to link_error should go away at compile-time.  */
extern void link_error (void);

#define TEST_ONE(SUFFIX, TYPE)			\
  void __attribute__ ((noinline, noclone))	\
  test##SUFFIX (TYPE f, int *res)		\
  {						\
    TYPE sqrt_res = sqrt##SUFFIX (f);		\
    res[0] = sqrt_res < 0;			\
    if (res[0])					\
      link_error ();				\
    res[1] = sqrt_res <= 0;			\
    if (res[1] != (f == 0))			\
      link_error ();				\
    res[2] = (sqrt_res == 0);			\
    if (res[2] != (f == 0))			\
      link_error ();				\
    res[3] = (sqrt_res != 0);			\
    if (res[3] != (f != 0))			\
      link_error ();				\
    res[4] = (sqrt_res > 0);			\
    if (res[4] != (f > 0))			\
      link_error ();				\
    res[5] = (sqrt_res >= 0);			\
    if (!res[5])				\
      link_error ();				\
  }

volatile float f;
volatile double d;
volatile long double ld;

TEST_ONE (f, float)
TEST_ONE (, double)
TEST_ONE (l, long double)

int
main ()
{
  int res[6];
  testf (f, res);
  test (d, res);
  testl (ld, res);
  return 0;
}
