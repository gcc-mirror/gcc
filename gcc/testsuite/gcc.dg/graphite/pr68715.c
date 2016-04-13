/* { dg-do compile } */
/* { dg-options "-O2 -floop-interchange" } */

int a[1], c[1];
int b, d, e;

void
fn1 (int p1)
{
  for (;;)
    ;
}

int
fn3 ()
{
  for (; e; e++)
    c[e] = 2;
  for (; d; d--)
    a[d] = 8;
  return 0;
}

int fn5 (int);

int
fn2 ()
{
  fn3 ();
}

void
fn4 ()
{
  fn1 (b || fn5 (fn2 ()));
}
