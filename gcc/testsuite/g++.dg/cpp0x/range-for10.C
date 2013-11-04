// PR c++/47388
// { dg-do compile }
// { dg-options "-fno-for-scope -std=c++11" }

template <int>
void
foo ()
{
  int a[] = { 1, 2, 3, 4 };
  for (int i : a)
    ;
}

void
bar ()
{
  foo <0> ();
}
