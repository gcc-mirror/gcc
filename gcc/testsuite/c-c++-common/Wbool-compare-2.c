/* PR c/64610 */
/* { dg-do compile } */
/* { dg-options "-Wbool-compare" } */

#ifndef __cplusplus
# define bool _Bool
# define true 1
# define false 0
#endif

extern bool foo (void);

enum { A, B };

int
fn1 (bool b)
{
  int r = 0;

  r += b >= 0; /* { dg-warning "with boolean expression is always true" } */
  r += b > 0;
  r += b < 0; /* { dg-warning "with boolean expression is always false" } */
  r += b <= 0;

  r += b >= 1;
  r += b > 1; /* { dg-warning "with boolean expression is always false" } */
  r += b < 1;
  r += b <= 1; /* { dg-warning "with boolean expression is always true" } */

  r += foo () >= 0; /* { dg-warning "with boolean expression is always true" } */
  r += foo () > 0;
  r += foo () < 0; /* { dg-warning "with boolean expression is always false" } */
  r += foo () <= 0;

  r += foo () >= 1;
  r += foo () > 1; /* { dg-warning "with boolean expression is always false" } */
  r += foo () < 1;
  r += foo () <= 1; /* { dg-warning "with boolean expression is always true" } */

  r += b >= A; /* { dg-warning "with boolean expression is always true" } */
  r += b > A;
  r += b < A; /* { dg-warning "with boolean expression is always false" } */
  r += b <= A;

  r += b >= B;
  r += b > B; /* { dg-warning "with boolean expression is always false" } */
  r += b < B;
  r += b <= B; /* { dg-warning "with boolean expression is always true" } */

  /* Swap LHS and RHS.  */
  r += 0 >= b;
  r += 0 > b; /* { dg-warning "with boolean expression is always false" } */
  r += 0 < b;
  r += 0 <= b; /* { dg-warning "with boolean expression is always true" } */

  r += 1 >= b; /* { dg-warning "with boolean expression is always true" } */
  r += 1 > b;
  r += 1 < b; /* { dg-warning "with boolean expression is always false" } */
  r += 1 <= b;

  r += 0 >= foo ();
  r += 0 > foo (); /* { dg-warning "with boolean expression is always false" } */
  r += 0 < foo ();
  r += 0 <= foo (); /* { dg-warning "with boolean expression is always true" } */

  r += 1 >= foo (); /* { dg-warning "with boolean expression is always true" } */
  r += 1 > foo ();
  r += 1 < foo (); /* { dg-warning "with boolean expression is always false" } */
  r += 1 <= foo ();

  r += A >= b;
  r += A > b; /* { dg-warning "with boolean expression is always false" } */
  r += A < b;
  r += A <= b; /* { dg-warning "with boolean expression is always true" } */

  r += B >= b; /* { dg-warning "with boolean expression is always true" } */
  r += B > b;
  r += B < b; /* { dg-warning "with boolean expression is always false" } */
  r += B <= b;

  return r;
}

int
fn2 (int i, int j)
{
  int r = 0;

  r += (i == j) >= 0; /* { dg-warning "with boolean expression is always true" } */
  r += (i == j) > 0;
  r += (i == j) < 0; /* { dg-warning "with boolean expression is always false" } */
  r += (i == j) <= 0;

  r += (i == j) >= 1;
  r += (i == j) > 1; /* { dg-warning "with boolean expression is always false" } */
  r += (i == j) < 1;
  r += (i == j) <= 1; /* { dg-warning "with boolean expression is always true" } */

  return r;
}
