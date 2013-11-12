// Generic lambda type dependence test part from N3690 5.1.2.12
// { dg-options "-std=c++1y" }

void f(int, const int (&)[2] = {}) { } // #1
void f(const int&, const int (&)[1]) { } // #2

void test()
{
  const int x = 17;
  auto g = [](auto a) {
    f(x); // OK: calls #1, does not capture x
  };
  auto g2 = [=](auto a) {
    int selector[sizeof(a) == 1 ? 1 : 2]{};
    f(x, selector); // OK: is a dependent expression, so captures x
  };
}

struct S {
  struct N {
    auto test () { return 7.f; }
  };
};

#include <utility>

int main()
{
  auto f = [] <typename T> (T const& s) mutable {
    typename T::N x;
    return x.test ();
  };
  auto g = [] (auto const& s) {
    typename std::decay<decltype (s)>::type::N x;
    return x.test ();
  };

  S i;
  f(i);
  g(i);
}

