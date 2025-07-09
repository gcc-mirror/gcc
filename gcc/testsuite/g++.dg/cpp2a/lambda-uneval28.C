// PR c++/121008
// { dg-do compile { target c++20 } }

struct A {
  void f()
    noexcept(noexcept([this]() noexcept(noexcept(this)) {}))
  {}
};

int main() {}
