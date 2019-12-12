// PR c++/87145
// { dg-do compile { target c++11 } }

struct S {
  int val;

  constexpr operator int() const {
    return static_cast<int>(val);
  }
};

template<int N>
struct F { };

template<unsigned N>
constexpr void foo() {
  F<int{N}> f;
  F<S{N}> f2;
}

int
main()
{
  foo<2>();
}
