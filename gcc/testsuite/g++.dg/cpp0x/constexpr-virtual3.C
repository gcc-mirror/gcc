// PR c++/54253
// { dg-do compile { target c++11 } }

namespace A {
  class Base {
    int x;
  public:
    constexpr Base(int x) : x(x) {}
  };

  class Base2 {
  public:
    virtual void fun() {}
  };

  class Derived : public Base2, public Base {
  public:
    constexpr Derived() : Base2(), Base(5) {}
  };

  constexpr Derived der;
}

namespace B {
  class Base {
    int x;
  public:
    constexpr Base() : x(5) {}
  };

  class Base2 {
  public:
    virtual void fun() {}
  };

  class Derived : public Base, public Base2 {
  public:
    constexpr Derived() {}
  };

  constexpr Derived der;
}
