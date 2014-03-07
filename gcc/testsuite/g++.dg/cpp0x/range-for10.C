// PR c++/47388
// { dg-do compile { target c++11 } }
// { dg-options "-fno-for-scope" }

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
