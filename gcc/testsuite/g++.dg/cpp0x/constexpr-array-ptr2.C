// { dg-do compile { target c++11 } }

template<class T>
struct IsNegative {
  int dummy; // Workaround for empty class problem
  constexpr IsNegative() : dummy(0) {}
  constexpr bool operator()(const T& x) {
    return x < T(0);
  }
};

template<class T, int N, class Pred>
constexpr bool has_neg(T (&x)[N], Pred p) {
  return p(x[0]) || p(x[1]);
}

constexpr int a[] = {1, -2};

constexpr auto answer = has_neg(a, IsNegative<int>{}); // #1

static_assert(answer, "Error");
