// P0614R1
// { dg-do compile }
// { dg-options "-std=c++2a" }

static const int a[] = { 1, 2, 3, 4, 5 };
extern void foo (int);
extern void bar (int, int);

constexpr int
baz ()
{
  return 6;
}

void
fn1 (int i)
{
  for ((i += 2); auto x : a)
    foo (i);

  for (auto j = 0, k = 0; auto x : a)
    bar (j + k, x);

  for (constexpr int j = baz (); auto x : a)
    bar (x, j);
}
