/* Test that all accumulator registers are accessible.  */
/* { dg-options "-mcpu=fr450" } */
/* { dg-do run } */
extern void abort (void);
extern void exit (int);

#define TEST_ACC(X) \
  (__MWTACC (X, 0x11220000 | X), __MRDACC (X) ^ (0x11220000 | X))

#define TEST_ACCG(X) \
  (__MWTACCG (X, X), __MRDACCG (X) ^ X)

#define ZERO_ACC(X) \
  (__MRDACC (X) | __MRDACCG (X))

int
main ()
{
  if (TEST_ACC (0) | TEST_ACC (1) | TEST_ACC (2) | TEST_ACC (3))
    abort ();
  if (TEST_ACC (8) | TEST_ACC (9) | TEST_ACC (10) | TEST_ACC (11))
    abort ();
  if (TEST_ACCG (0) | TEST_ACCG (1) | TEST_ACCG (2) | TEST_ACCG (3))
    abort ();
  if (TEST_ACCG (8) | TEST_ACCG (9) | TEST_ACCG (10) | TEST_ACCG (11))
    abort ();

  __MCLRACCA ();
  if (ZERO_ACC (0) | ZERO_ACC (1) | ZERO_ACC (2) | ZERO_ACC (3))
    abort ();
  if (ZERO_ACC (8) | ZERO_ACC (9) | ZERO_ACC (10) | ZERO_ACC (11))
    abort ();

  exit (0);
}
