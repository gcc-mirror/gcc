// P2662R3 - Pack Indexing
// PR c++/113798
// { dg-do compile { target c++26 } }

template <int I, auto...Ts>
decltype(Ts...[I])
foo ()			      // { dg-bogus "sorry, unimplemented: mangling" "" { xfail *-*-* } }
{
  return Ts...[I];
}

int
g ()
{
  return foo<2, 0, 1, 42>();
}
