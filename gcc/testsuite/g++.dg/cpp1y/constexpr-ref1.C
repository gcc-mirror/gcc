// PR c++/64080
// { dg-do compile { target c++14 } }

constexpr void foo (int&& i) { i = 0; }

void bar(int&& i)
{
  bool b = noexcept(foo(static_cast<int&&>(i)));
}
