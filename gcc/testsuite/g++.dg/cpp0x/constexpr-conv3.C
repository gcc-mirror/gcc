// PR c++/87145
// { dg-do compile { target c++11 } }

template<typename T, T t> struct integral_constant {
  static constexpr T value = t;
};

enum class Enum : unsigned {};

struct Pod {
  unsigned val;

  constexpr operator Enum() const {
    return static_cast<Enum>(val);
  }
};

template<unsigned N>
constexpr void foo() {
  using Foo = integral_constant<Enum, Pod{N}>;
}

int main() {
  foo<2>();
}
