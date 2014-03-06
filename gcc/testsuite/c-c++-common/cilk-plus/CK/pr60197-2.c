/* PR c/60197 */
/* { dg-do compile } */
/* { dg-options "-fcilkplus" } */

extern int foo (void);

int
fn1 (void)
{
  int i;
  i = (_Cilk_spawn foo ()) + 1; /* { dg-error "invalid use of" } */
  return i;
}

int
fn2 (void)
{
  int i = (_Cilk_spawn foo ()) + 1; /* { dg-error "invalid use of" } */
  return i;
}

int
fn3 (int j, int k, int l)
{
  int i = (((((_Cilk_spawn foo ()) + 1) - l) * k) / j); /* { dg-error "invalid use of" } */
  return i;
}

int
fn4 (int j, int k, int l)
{
  int i;
  i = (((((_Cilk_spawn foo ()) + 1) - l) * k) / j); /* { dg-error "invalid use of" } */
  return i;
}
