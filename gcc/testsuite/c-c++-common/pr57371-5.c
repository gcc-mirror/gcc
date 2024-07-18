/* { dg-do compile } */
/* { dg-options "-O -fno-signaling-nans -fno-trapping-math -fdump-tree-original" } */

/* We can not get rid of comparison in tests below because of
   pending NaN exceptions.  */

#define QNAN __builtin_nanf ("0")
#define SNAN __builtin_nansf ("0")

void nonfinite(unsigned short x) {
  {
    volatile int nonfinite_1;
    nonfinite_1 = (float) x > QNAN;
    /* { dg-final { scan-tree-dump "nonfinite_1 = 0" "original" } } */
  }

  {
    volatile int nonfinite_2;
    nonfinite_2 = (float) x >= QNAN;
    /* { dg-final { scan-tree-dump "nonfinite_2 = 0" "original" } } */
  }

  {
    volatile int nonfinite_3;
    nonfinite_3 = (float) x < QNAN;
    /* { dg-final { scan-tree-dump "nonfinite_3 = 0" "original" } } */
  }

  {
    volatile int nonfinite_4;
    nonfinite_4 = (float) x <= QNAN;
    /* { dg-final { scan-tree-dump "nonfinite_4 = 0" "original" } } */
  }

  {
    volatile int nonfinite_5;
    nonfinite_5 = (float) x > SNAN;
    /* { dg-final { scan-tree-dump "nonfinite_5 = 0" "original" } } */
  }

  {
    volatile int nonfinite_6;
    nonfinite_6 = (float) x >= SNAN;
    /* { dg-final { scan-tree-dump "nonfinite_6 = 0" "original" } } */
  }

  {
    volatile int nonfinite_7;
    nonfinite_7 = (float) x < SNAN;
    /* { dg-final { scan-tree-dump "nonfinite_7 = 0" "original" } } */
  }

  {
    volatile int nonfinite_8;
    nonfinite_8 = (float) x <= SNAN;
    /* { dg-final { scan-tree-dump "nonfinite_8 = 0" "original" } } */
  }

  {
    volatile int nonfinite_9;
    nonfinite_9 = (float) x == SNAN;
    /* { dg-final { scan-tree-dump "nonfinite_9 = 0" "original" } } */
  }

  {
    volatile int nonfinite_10;
    nonfinite_10 = (float) x != SNAN;
    /* { dg-final { scan-tree-dump "nonfinite_10 = 1" "original" } } *
 */
  }

  {
    volatile int nonfinite_11;
    nonfinite_11 = (float) x == QNAN;
    /* { dg-final { scan-tree-dump "nonfinite_11 = 0" "original" } } */
  }

  {
    volatile int nonfinite_12;
    nonfinite_12 = (float) x != QNAN;
    /* { dg-final { scan-tree-dump "nonfinite_12 = 1" "original" } } */
  }
}
