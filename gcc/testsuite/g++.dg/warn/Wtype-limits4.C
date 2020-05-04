// PR c++/94938
// { dg-additional-options "-Wtype-limits" }

template<unsigned N> struct B { unsigned arr[N]; };
template<> struct B<1u> { int arr[10]; };

template <unsigned N> bool
foo(B<N> l)
{
  int i = 0;
  return l.arr[i] < 0;
}

void
j()
{
  B<1u> b;
  foo (b);
  B<2u> b2;
  // I think that in this instantiation we could warn, but it breaks
  // gcc bootstrap (marek 5/2020).
  foo (b2);
}
