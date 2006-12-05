// PR c++/29728

template<int> void foo()
{
  int a[] = { X: 0 }; // { dg-error "designated initializer" }
}
