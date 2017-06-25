// PR c++/80465
// { dg-options -std=c++1z }

int foo(...);
int main() {
  [](auto a) noexcept(noexcept(foo(a))){}(42);
}
