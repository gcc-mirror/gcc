// PR c++/115012
// { dg-do compile { target { c++11 } } }
// { dg-final { scan-assembler "_Z3fooIJidEEvDpFT_iE" } }
// { dg-final { scan-assembler "_Z3barIiEvPFT_iE" } }
// { dg-final { scan-assembler "_Z3bazIJidEEvDpFT_iE" } }

template <typename ...T>
void
foo (T... x (int))
{
}

template <typename T>
void
bar (T (int))
{
}

template <typename ...T>
void
baz (T... (int))
{
}

int
f1 (int)
{
  return 0;
}

double
f2 (int)
{
  return 0;
}

void
corge ()
{
  foo <int, double> (f1, f2);
  bar <int> (f1);
  baz <int, double> (f1, f2);
}
