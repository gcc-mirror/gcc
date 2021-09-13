// { dg-do compile { target c++11 } }
// { dg-additional-options "-O1 -Wno-analyzer-use-of-uninitialized-value" }

template <typename DV> DV
vu (DV j4)
{
  return [j4] () { return j4 () ? j4 : throw j4 (); } ();
}

void
foo ()
{
  auto n1 = [] { return nullptr; };

  vu (n1);
}
