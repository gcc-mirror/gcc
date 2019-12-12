// PR c++/89089
// { dg-do compile { target c++11 } }

template <typename...> struct A {};
template <typename T, typename... U> struct A<T, U...> {
private:
  [[no_unique_address]] A<U...> a;
};
struct B {
  template <typename... U> A<U...> operator()(U...) { return A<U...>(); }
} f;
auto fn = f (int{}, [] {});
