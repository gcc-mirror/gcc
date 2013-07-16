// PR c++/54277
// { dg-do compile { target c++11 } }

struct Used
{
  void foo() { }
};

template <typename>
struct S
{
  Used x;

  void bar()
  {
    auto f = [this] { x.foo(); };
    f();
  }
};
