/* { dg-do compile } */
/* { dg-options "-Ofast -floop-interchange" } */

int a, b, c, d, f, g;
int e[1], h[1];
void fn2 ();
void fn3 ();
void
fn1 ()
{
  fn2 ();
  b = 0;
  for (; b < 10; b++)
    ;
}

void
fn2 ()
{
  if (a)
    {
      fn3 ();
      c = d;
    }
}

void
fn3 ()
{
  for (; g; g++)
    e[g] = 2;
  if (f)
    for (; g; g++)
      h[g] = 5;
}
