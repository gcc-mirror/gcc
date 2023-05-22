// PR c++/109745
// { dg-do run { target c++14 } }
// { dg-additional-options "-O" }

template<class T>
struct Foo { T val; };

struct Bar {
  constexpr Bar() = default;
  constexpr Bar(Bar const& other) { other.val_ = 42; }
  constexpr int val() const { return val_; }
  mutable int val_{};
};

int main() {
  constexpr Foo<Bar> x{};
  Foo<Bar> y{x};
  if (x.val.val() != 42 || x.val.val_ != 42)
    __builtin_abort();
}
