// P2036R3 - Change scope of lambda trailing-return-type
// PR c++/102610
// { dg-do compile { target c++23 } }

template <typename T, typename U>
constexpr bool is_same = false;

template <typename T>
constexpr bool is_same<T, T> = true;

struct S {
  void foo () {
    auto counter1 = [j=0]() mutable -> decltype(j) {
      return j++;
    };
    auto counter2 = [j=0, o=0, k=0, e=0]() mutable -> decltype(j) {
      return j + o + k + e;
    };
  }
};

// [expr.prim.id.unqual]/3.2
void
f ()
{
  float x, &r = x;

  [=]() -> decltype((x)) {      // lambda returns float const& because this lambda is not mutable and
                                // x is an lvalue
    decltype(x) y1;             // y1 has type float
    decltype((x)) y2 = y1;      // y2 has type float const&
    decltype(r) r1 = y1;        // r1 has type float&
    decltype((r)) r2 = y2;      // r2 has type float const&
    return y2;
  };

  [=](decltype((x)) y) {
    decltype((x)) z = x;        // OK, y has type float&, z has type float const&
    static_assert(is_same<float&, decltype((y))>);
    static_assert(is_same<const float&, decltype((z))>);
  };

  [=] {
    [](decltype((x)) y) {     // OK, lambda takes a parameter of type float const&
    };

    [x=1](decltype((x)) y) {
      decltype((x)) z = x;      // OK, y has type int&, z has type int const&
      // FIXME We don't handle nested lambdas yet?
      //static_assert(is_same<int&, decltype((y))>);
      static_assert(is_same<const int&, decltype((z))>);
    };
  };

  [x=1](decltype((x)) y) {
    decltype((x)) z = x;
    static_assert(is_same<int&, decltype((y))>);
    static_assert(is_same<const int&, decltype((z))>);
  };
}

void
ok ()
{
  auto counter1 = [j=0]() mutable -> decltype(j) {
    static_assert(is_same<int&, decltype((j))>);
    return j++;
  };

  auto l = [j=0]() -> decltype(j) {
    static_assert(is_same<const int&, decltype((j))>);
    return j;
  };

  int y;
  [=] -> decltype((y)) {
    return y;
  };
}

void
foo ()
{
  int x = [x](int y[sizeof x]){return sizeof x;}(0);
}
