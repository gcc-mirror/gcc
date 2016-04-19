// PR c++/66543
// { dg-do compile { target c++14 } }
// { dg-options "-Wunused-but-set-variable" }

int main() {
  auto f = []() { };
  [=](auto) {
    using Foo = decltype(f());
  };
}
