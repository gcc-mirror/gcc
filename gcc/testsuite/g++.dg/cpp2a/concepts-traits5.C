// PR c++/121859
// { dg-do compile { target c++20 } }

template <typename T>
struct S {
  template <typename U>
  static constexpr bool foo = sizeof(T) == sizeof(U);
};

template <typename U> void bar(U x) requires S<char>::foo<U> {}

int main() {
  bar(double{});  // { dg-error "no matching function" }
}
