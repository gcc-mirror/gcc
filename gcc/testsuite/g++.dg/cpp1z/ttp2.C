// PR c++/95036
// { dg-do compile { target c++17 } }

namespace a {
template <int b> struct c { static constexpr int d = b; };
template <bool, typename e = void> using f = e;
template <typename e, e...> struct g;
template <typename> struct array;
} // namespace a
template <template <class> class h> struct i {
  template <template <class, auto...> class, class = void> struct n;
  template <class j> struct n<h, j> : a::c<true> {};
  template <template <class> class k, class = a::f<n<k>::d>> void function();
};
template <template <class> class... l> struct derived : i<l>... {
  using i<l>::function...;
};
int main() {
  derived<a::array, a::g> m;
  m.function<a::array>();
}
