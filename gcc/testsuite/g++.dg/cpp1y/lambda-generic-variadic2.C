// PR c++/64105
// This test was ICEing on pre-C++14 mode.

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
  X::f([](auto... xs){});	// { dg-error "" "" { target { ! cxx14 } } }
};
