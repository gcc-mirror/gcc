// PR c++/108526
// { dg-do compile { target c++23 } }
// { dg-additional-options -g } PR108706

template<class> void f()
{
  auto a = [] (auto x) static { return x; };
}
template void f<int>();
