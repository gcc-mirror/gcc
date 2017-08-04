/* { dg-do compile } */
/* { dg-options "-O -fsignaling-nans -fdump-tree-original" } */

/* We can not get rid of comparison in tests below because of
   pending NaN exceptions.

   TODO: avoid under -fno-trapping-math.  */

#define QNAN __builtin_nanf ("0")
#define SNAN __builtin_nansf ("0")

void nonfinite(unsigned short x) {
  {
    volatile int nonfinite_1;
    nonfinite_1 = (float) x > QNAN;
    /* { dg-final { scan-tree-dump "nonfinite_1 = \\(float\\)" "original" } } */
  }

  {
    volatile int nonfinite_2;
    nonfinite_2 = (float) x >= QNAN;
    /* { dg-final { scan-tree-dump "nonfinite_2 = \\(float\\)" "original" } } */
  }

  {
    volatile int nonfinite_3;
    nonfinite_3 = (float) x < QNAN;
    /* { dg-final { scan-tree-dump "nonfinite_3 = \\(float\\)" "original" } } */
  }

  {
    volatile int nonfinite_4;
    nonfinite_4 = (float) x <= QNAN;
    /* { dg-final { scan-tree-dump "nonfinite_4 = \\(float\\)" "original" } } */
  }

  {
    volatile int nonfinite_5;
    nonfinite_5 = (float) x > SNAN;
    /* { dg-final { scan-tree-dump "nonfinite_5 = \\(float\\)" "original" } } */
  }

  {
    volatile int nonfinite_6;
    nonfinite_6 = (float) x >= SNAN;
    /* { dg-final { scan-tree-dump "nonfinite_6 = \\(float\\)" "original" } } */
  }

  {
    volatile int nonfinite_7;
    nonfinite_7 = (float) x < SNAN;
    /* { dg-final { scan-tree-dump "nonfinite_7 = \\(float\\)" "original" } } */
  }

  {
    volatile int nonfinite_8;
    nonfinite_8 = (float) x <= SNAN;
    /* { dg-final { scan-tree-dump "nonfinite_8 = \\(float\\)" "original" } } */
  }

  {
    volatile int nonfinite_9;
    nonfinite_9 = (float) x == SNAN;
    /* { dg-final { scan-tree-dump "nonfinite_9 = \\(float\\)" "original" } } */
  }

  {
    volatile int nonfinite_10;
    nonfinite_10 = (float) x != SNAN;
    /* { dg-final { scan-tree-dump "nonfinite_10 = \\(float\\)" "original" } } */
  }
}
