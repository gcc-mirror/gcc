// PR c++/87152
// { dg-do run }
// { dg-options "-std=c++2a" }

template<typename T>
void
fn ()
{
  T a[] = { 1, 2, 3, 4, 5 };

  for (T i = []{ return 3; }(); auto x : a)
    if (i != 3)
      __builtin_abort ();

  for (T i = ({ 3; }); auto x : a)
    if (i != 3)
      __builtin_abort ();
}

int
main ()
{
  fn<int>();
}
