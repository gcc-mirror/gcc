// { dg-do compile { target c++11 } }

constexpr bool is_negative(int x) {
  return x < 0;
}

constexpr bool do_has_neg(const int* x, bool(*p)(int)) {
 return p(x[0]) || p(x[1]);  // Line 6
}

constexpr bool has_neg(const int (&x)[2], bool(*p)(int)) {
 return do_has_neg(x, p); // Line 10
}

constexpr int a[] = {1, -2};

constexpr auto answer = has_neg(a, is_negative); // Line 15

static_assert(answer, "Error");
