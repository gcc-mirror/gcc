// PR c++/108525
// { dg-do compile { target c++23 } }

auto b = [](...) static { return 1; };
auto foo () { return b (); }
// PR c++/108526
// { dg-do compile { target c++23 } }

template<class> void f()
{
  auto a = [] (auto x) static { return x; };
}
template void f<int>();
