// PR c++/58503
// { dg-require-effective-target c++11 }

template<int> void foo()
{
  for (auto i : 0) {}  // { dg-error "there are no arguments" }
}
