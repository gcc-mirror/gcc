// PR c++/128386
// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection -fsanitize=undefined" }
// Test consteval-only values are stripped before ubsan pass.

using info = decltype(^^int);

struct refl_span {
  const info* data;
  int length;
  consteval auto begin() const { return data; }
  consteval auto end() const { return data + length; }
};

constexpr info arr[] = {^^int, ^^float, ^^char};
constexpr refl_span sp{arr, 3};

template<typename = void>
constexpr int foo(int x) {
  template for (constexpr auto _ : sp) {}
  return x;
}

int main() {
  int x = 1;
  (void)foo(x);
}
