// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test from [temp.dep.splice].

template <auto T, auto NS>
void fn() {
  using a = [:T:]<1>;  // [:T:] and [:T:]<1> are dependent

  static_assert([:NS:]::template TCls<1>::v == a::v);  // [:NS:] is dependent
}

namespace NS {
template <auto V> struct TCls { static constexpr int v = V; };
}

int main() {
  fn<^^NS::TCls, ^^NS>();
}
