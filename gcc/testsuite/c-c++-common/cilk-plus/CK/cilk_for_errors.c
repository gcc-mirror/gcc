/* { dg-do compile } */
/* { dg-options "-fcilkplus" } */
/* { dg-additional-options "-std=c99" { target c } } */


int main (void)
{
  int q = 0, ii = 0, jj = 0;

  _Cilk_for (int ii; ii < 10; ii++) /* { dg-error "is not initialized" "" { target c } } */
    /* { dg-error "expected" "" { target c++ } .-1 } */
    q = 5;

  _Cilk_for (; ii < 10; ii++) /* { dg-error "expected iteration declaration" } */
    q = 2;

  _Cilk_for (int ii = 0; ; ii++) /* { dg-error "missing controlling predicate" } */
    q = 2;

  _Cilk_for (int ii = 0; ii < 10, jj < 10; ii++)  /* { dg-error "expected ';' before ',' token" "" { target c } } */
    /* { dg-error "invalid controlling predicate" "" { target c++ }  .-1 } */
    q = 5;

  _Cilk_for (int ii = 0; ii < 10; ) /* { dg-error "missing increment" } */
    q = 5;

  _Cilk_for (int ii = 0, jj = 0; ii < 10; ii++) /* { dg-error "expected|invalid" } */
    q = 5;

  _Cilk_for (volatile int vii = 0; vii < 10; vii++) /* { dg-error "iteration variable cannot be volatile" } */
    q = 5;

  _Cilk_for (static int sii = 0; sii < 10; sii++) /* { dg-error "static|expected|declared|expression" } */
    q = 5;

  _Cilk_for (float fii = 3.47; fii < 5.23; fii++) /* { dg-error "invalid type for iteration variable" } */
    q = 5;

  _Cilk_for (int ii = 0; 10 > jj; ii++) /* { dg-error "invalid controlling predicate" } */
    q = 5;

  _Cilk_for (int ii = 0; ii < 10; ii >> 1) /* { dg-error "invalid increment expression" } */
    q = 5;

  _Cilk_for (int ii = 10; ii >= 0; ii--) /* This is OK!  */
    q = 5;

  _Cilk_for (int ii; ii < 10; ii++) /* { dg-error "is not initialized" "" { target c } } */
    /* { dg-error "expected" "" { target c++ } .-1 } */
    q = 5;

  return 0;
}
