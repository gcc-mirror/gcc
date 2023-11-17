// PR c++/82643
// { dg-do compile { target c++14 } }

int main()
{
  struct A {
    constexpr int operator()() const { return 42; }
  };

  auto f = A();
  constexpr auto x = f(); //ok, call constexpr const non-static method

  [](auto const &f) {
    constexpr auto x = f();
  }(f);

  [&]() {
    constexpr auto x = f(); //ko, __closure is not a constant expression
  };

  [=]() {
    constexpr auto x = f(); //same ko, __closure is not a constant expression
  };
}
