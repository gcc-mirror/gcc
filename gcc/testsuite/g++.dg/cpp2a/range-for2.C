// P0614R1
// { dg-do compile }
// { dg-options "-std=c++2a" }

void
fn1 ()
{
  int a[] = { 1, 2, 3, 4, 5 };

  for (int i = 0; auto x : a)
    ++i;

  int i;
  for (i = 0; auto x : a)
    ++i;
}
