// { dg-options -std=c++0x }

template<class T>
constexpr T do_get(T* x, int n) {
  return x[n - 1];
}

template<class T, int N>
constexpr T get(T (&x)[N]) {
  return do_get(x, N);
}

constexpr int arr_i[] = {1};
constexpr auto var = get(arr_i); // #2
static_assert(var == arr_i[0], "Error");
