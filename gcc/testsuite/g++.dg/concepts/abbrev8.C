// PR c++/98990
// { dg-do compile { target concepts } }

int x;

auto& f() { return x; }
auto& f(auto) { return x; }

using T1 = int&;
using T1 = decltype(f('a'));

int* y;

template <class>
struct S
{
  static auto** f() { return &y; }
  static auto** f(auto) { return &y; }
};

using T2 = int**;
using T2 = decltype(S<void>::f('a'));
