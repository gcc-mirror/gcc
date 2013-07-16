/* PR c++/56607 */
/* { dg-do compile { target { { lp64 || ilp32 } || llp64 } } } */
/* { dg-options "-O2 -Wdiv-by-zero" } */

int
f1 (void)
{
  return 1 / (sizeof (char) - 1);	/* { dg-warning "division by zero" } */
}

int
f2 (void)
{
  const int x = sizeof (char) - 1;
  return 1 / x;				/* { dg-warning "division by zero" "" { target c++ } } */
}

int
f3 (void)
{
  return 1 / (sizeof (int) / 3 - 1);	/* { dg-warning "division by zero" } */
}

int
f4 (void)
{
  const int x = sizeof (int) / 3 - 1;
  return 1 / x;				/* { dg-warning "division by zero" "" { target c++ } } */
}
