// PR c++/80465
// { dg-do compile { target c++17 } }

int foo(...);
int main() {
  [](auto a) noexcept(noexcept(foo(a))){}(42);
}
