// PR c++/80767
// { dg-do compile { target c++11 } }

template <typename T, typename U = T> struct A { using type = U; };
template <typename F, typename... G> struct B : B<F>::type, B<G...>::type {
  using type = B;
  using B<F>::type::operator();
};
template <typename F> struct B<F> { using type = F; };
struct {
  template <typename... F,
            typename Overload = typename B<typename A<F>::type...>::type>
  Overload operator()(F...){}
} a;
int main() {
  auto f = a([](int) {}, [](float) {});
  f({});
}
