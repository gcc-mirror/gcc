/* { dg-do compile } */
/* { dg-options "-O3" } */

int a, b, c;

struct S
{
  int f0;
  int f1;
} d;

static int fn2 (struct S);
void fn3 (struct S);

void
fn1 (struct S p)
{
  struct S h = { 0, 0 };
  fn3 (p);
  fn2 (h);
}

int
fn2 (struct S p)
{
  struct S j = { 0, 0 };
  fn3 (p);
  fn2 (j);
  return 0;
}

void
fn3 (struct S p)
{
  for (; b; a++)
    c = p.f0;
  fn1 (d);
}

void
fn4 ()
{
  for (;;)
    {
      struct S f = { 0, 0 };
      fn1 (f);
    }
}
