// PR c++/93413
// { dg-do compile { target c++20 } }

struct Base {
  virtual ~Base() = default;
};
constexpr Base b;

struct Derived : Base { };
constexpr Derived d;
