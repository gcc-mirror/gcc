// { dg-options -std=c++0x }

template<int I> void f() {
  for (auto i: {I} );
}
