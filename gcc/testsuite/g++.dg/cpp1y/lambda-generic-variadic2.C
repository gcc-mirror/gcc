// PR c++/64105
// This test was ICEing in C++11 mode.
// { dg-do compile { target c++11 } }
// { dg-skip-if "requires hosted libstdc++ for functional function" { ! hostedlib } }

#include <functional>

using F = std::function<void(void)>;

struct X
{
  template <typename T>
  static void f(T t)
  {
    g(t);
  }

  static void g(F) {}
};

int main()
{
  X::f([](auto... xs){});	// { dg-error "" "" { target { ! c++14 } } }
}
