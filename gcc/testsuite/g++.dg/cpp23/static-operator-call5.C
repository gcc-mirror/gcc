// PR c++/108526
// { dg-do compile { target c++23 } }

template<class> void f()
{
  auto a = [] (auto x) static { return x; };
}
template void f<int>();
