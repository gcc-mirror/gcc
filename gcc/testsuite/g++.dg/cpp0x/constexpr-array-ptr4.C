// { dg-options -std=c++0x }

constexpr const int do_last(const int* x, int n) {
 return x[n - 1];
}

struct IsNegative {
  constexpr bool operator()(const int& x) {
    return x < 0;
  }
};

template<int N, class Pred>
constexpr bool has_neg(const int (&x)[N], Pred p) {
  return p(do_last(x, N)); // Line 13
}

constexpr int a[] = {1, -2};

constexpr auto answer = has_neg(a, IsNegative{}); // Line 18

static_assert(answer, "Error");

