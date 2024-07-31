// PR c++/115987
// { dg-do compile { target c++11 } }
// { dg-options "-Wdangling-reference" }

template <typename T>
struct Wrapper {
    T val;
};

template <typename T, typename FUNC>
    const T& unwrap_2(const Wrapper<T>& r, FUNC&&) {
  return r.val;
}

int main(int, char**) {
  const Wrapper<int> w{1234};
  const auto& u = unwrap_2(w, 1L);  // { dg-bogus "dangling reference" }
  __builtin_printf("Unwrapped value : %d\n", u);
}
