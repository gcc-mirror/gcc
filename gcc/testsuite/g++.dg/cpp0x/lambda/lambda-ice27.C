// PR c++/84446
// { dg-do compile { target c++11 } }

template<int> void foo()
{
  int i,
  i = [] { virtual }();  // { dg-error "redeclaration|expected" }
}
