/* PR c++/62153 */
/* { dg-do compile } */
/* { dg-options "-Wall" } */

#ifndef __cplusplus
# define bool _Bool
# define true 1
# define false 0
#endif

extern bool foo (void);
bool r;

enum { E = 4 };

void
fn1 (bool b)
{
  r = b == 2; /* { dg-warning "with boolean expression is always false" } */
  r = b != 2; /* { dg-warning "with boolean expression is always true" } */
  r = b < 2; /* { dg-warning "with boolean expression is always true" } */
  r = b > 2; /* { dg-warning "with boolean expression is always false" } */
  r = b <= 2; /* { dg-warning "with boolean expression is always true" } */
  r = b >= 2; /* { dg-warning "with boolean expression is always false" } */

  r = b == -1; /* { dg-warning "with boolean expression is always false" } */
  r = b != -1; /* { dg-warning "with boolean expression is always true" } */
  r = b < -1; /* { dg-warning "with boolean expression is always false" } */
  r = b > -1; /* { dg-warning "with boolean expression is always true" } */
  r = b <= -1; /* { dg-warning "with boolean expression is always false" } */
  r = b >= -1; /* { dg-warning "with boolean expression is always true" } */

  r = foo () == 2; /* { dg-warning "with boolean expression is always false" } */
  r = foo () != 2; /* { dg-warning "with boolean expression is always true" } */
  r = foo () < 2; /* { dg-warning "with boolean expression is always true" } */
  r = foo () > 2; /* { dg-warning "with boolean expression is always false" } */
  r = foo () <= 2; /* { dg-warning "with boolean expression is always true" } */
  r = foo () >= 2; /* { dg-warning "with boolean expression is always false" } */

  r = b == E; /* { dg-warning "with boolean expression is always false" } */
  r = b != E; /* { dg-warning "with boolean expression is always true" } */
  r = b < E; /* { dg-warning "with boolean expression is always true" } */
  r = b > E; /* { dg-warning "with boolean expression is always false" } */
  r = b <= E; /* { dg-warning "with boolean expression is always true" } */
  r = b >= E; /* { dg-warning "with boolean expression is always false" } */

  /* Swap LHS and RHS.  */
  r = 2 == b; /* { dg-warning "with boolean expression is always false" } */
  r = 2 != b; /* { dg-warning "with boolean expression is always true" } */
  r = 2 < b; /* { dg-warning "with boolean expression is always false" } */
  r = 2 > b; /* { dg-warning "with boolean expression is always true" } */
  r = 2 <= b; /* { dg-warning "with boolean expression is always false" } */
  r = 2 >= b; /* { dg-warning "with boolean expression is always true" } */

  r = -1 == b; /* { dg-warning "with boolean expression is always false" } */
  r = -1 != b; /* { dg-warning "with boolean expression is always true" } */
  r = -1 < b; /* { dg-warning "with boolean expression is always true" } */
  r = -1 > b; /* { dg-warning "with boolean expression is always false" } */
  r = -1 <= b; /* { dg-warning "with boolean expression is always true" } */
  r = -1 >= b; /* { dg-warning "with boolean expression is always false" } */

  r = E == b; /* { dg-warning "with boolean expression is always false" } */
  r = E != b; /* { dg-warning "with boolean expression is always true" } */
  r = E < b; /* { dg-warning "with boolean expression is always false" } */
  r = E > b; /* { dg-warning "with boolean expression is always true" } */
  r = E <= b; /* { dg-warning "with boolean expression is always false" } */
  r = E >= b; /* { dg-warning "with boolean expression is always true" } */

  /* These are of course fine.  */
  r = b == false;
  r = b != false;
  r = b == true;
  r = b != true;

  r = b <= false;
  r = b > false;
  r = b < true;
  r = b >= true;
}

void
fn2 (int i1, int i2)
{
  r = (i1 == i2) == 2; /* { dg-warning "with boolean expression is always false" } */
  r = (i1 == i2) != 2; /* { dg-warning "with boolean expression is always true" } */
  r = (i1 == i2) < 2; /* { dg-warning "with boolean expression is always true" } */
  r = (i1 == i2) > 2; /* { dg-warning "with boolean expression is always false" } */
  r = (i1 == i2) <= 2; /* { dg-warning "with boolean expression is always true" } */
  r = (i1 == i2) >= 2; /* { dg-warning "with boolean expression is always false" } */

  r = (i1 == i2) == -1; /* { dg-warning "with boolean expression is always false" } */
  r = (i1 == i2) != -1; /* { dg-warning "with boolean expression is always true" } */
  r = (i1 == i2) < -1; /* { dg-warning "with boolean expression is always false" } */
  r = (i1 == i2) > -1; /* { dg-warning "with boolean expression is always true" } */
  r = (i1 == i2) <= -1; /* { dg-warning "with boolean expression is always false" } */
  r = (i1 == i2) >= -1; /* { dg-warning "with boolean expression is always true" } */

  r = (i1 == i2) == E; /* { dg-warning "with boolean expression is always false" } */
  r = (i1 == i2) != E; /* { dg-warning "with boolean expression is always true" } */
  r = (i1 == i2) < E; /* { dg-warning "with boolean expression is always true" } */
  r = (i1 == i2) > E; /* { dg-warning "with boolean expression is always false" } */
  r = (i1 == i2) <= E; /* { dg-warning "with boolean expression is always true" } */
  r = (i1 == i2) >= E; /* { dg-warning "with boolean expression is always false" } */
}

void
fn3 (int n, bool b)
{
  /* Don't warn here.  */
  r = b == n;
  r = b != n;
  r = b < n;
  r = b > n;
  r = b <= n;
  r = b >= n;

  r = n == E;
  r = n != E;
  r = n < E;
  r = n > E;
  r = n <= E;
  r = n >= E;
}
