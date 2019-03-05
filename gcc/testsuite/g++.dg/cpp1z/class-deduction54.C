// PR c++/82152
// { dg-do compile { target c++17 } }

struct Base {};

template<typename T>
struct Derived : public Base {
  using Base::Base;
};

Derived() -> Derived< void >;

int main() {
  Derived x;
}
