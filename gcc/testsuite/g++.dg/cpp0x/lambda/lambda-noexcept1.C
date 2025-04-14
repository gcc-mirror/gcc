// PR c++/119764
// { dg-do compile { target c++11 } }
// { dg-additional-options "-fabi-version=0 -Wabi=20" }

int main() {
  const int x = 123;
  auto a = [&]() { return x; };
  auto b = [&]() noexcept { return x; }; // { dg-warning "no longer captured" }
  static_assert(sizeof(a) == sizeof(b), "");
}
