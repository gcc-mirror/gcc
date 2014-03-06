/* PR c/60197 */
/* { dg-do compile } */
/* { dg-options "-fcilkplus" } */

extern int foo (void);
extern int bar (int);

int
fn1 (void)
{
  return (_Cilk_spawn foo ()) * 2; /* { dg-error "in a return statement is not allowed" } */
}

int
fn2 (void)
{
  return (_Cilk_spawn foo ()) > 2; /* { dg-error "in a return statement is not allowed" } */
}

int
fn3 (int i, int j, int k)
{
  return ((((((_Cilk_spawn foo () + i) - j) * k) / j) | i) ^ k) ; /* { dg-error "in a return statement is not allowed" } */
}

int
fn4 (int i, int j, int k)
{
  return (((((i - _Cilk_spawn foo ()) * k) / j) | i) ^ k); /* { dg-error "in a return statement is not allowed" } */
}

int
fn5 (void)
{
  return _Cilk_spawn foo (); /* { dg-error "in a return statement is not allowed" } */
}

int
fn6 (void)
{
  return _Cilk_spawn foo () + _Cilk_spawn foo (); /* { dg-error "in a return statement is not allowed" } */
}

int
fn7 (void)
{
  return 5 % _Cilk_spawn foo (); /* { dg-error "in a return statement is not allowed" } */
}

int
fn8 (void)
{
  return !_Cilk_spawn foo (); /* { dg-error "in a return statement is not allowed" } */
}

int
fn9 (void)
{
  return foo () && _Cilk_spawn foo (); /* { dg-error "in a return statement is not allowed" } */
}

int
fn10 (void)
{
  return bar (_Cilk_spawn foo ()); /* { dg-error "in a return statement is not allowed" } */
}
