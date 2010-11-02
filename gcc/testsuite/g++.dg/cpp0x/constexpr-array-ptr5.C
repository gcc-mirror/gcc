// { dg-options -std=c++0x }

template<class T>
constexpr T do_last(T* x, int n) {
 return x[n - 1]; //
}

template<class T, int N>
constexpr T last(T (&x)[N]) {
 return do_last(x, N);
}

constexpr bool is_negative(int x) { return x < 0; }

template<class T>
struct IsNegative {
  constexpr bool operator()(const T& x) {
    return x < T(0);
  }
};

template<class T, int N, class Pred>
constexpr bool has_neg(T (&x)[N], Pred p) {
  return p(last(x)); // Line 22
}

constexpr int a[] = {1, -2};

constexpr auto answer1 = has_neg(a, IsNegative<int>{}); // Line 27
constexpr auto answer2 = has_neg(a, is_negative);

static_assert(answer2 == answer1, "Error");
