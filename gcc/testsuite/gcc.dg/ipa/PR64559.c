/* { dg-do compile } */
/* { dg-options "-Os"  } */

int a, b, c, d;

struct S
{
  int f0;
};

static int
fn1 (int p)
{
  return p == 0 || a;
}

static int
fn2 ()
{
  d = fn1 (c);
  return 0;
}

static int
fn3 (struct S p)
{
  p.f0 || fn2 ();
  if (fn1 (1))
    b = 0;
  return 0;
}

int
main ()
{
  struct S e = { 1 };
  fn3 (e);
  return 0;
}
