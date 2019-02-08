// PR c++/88752
// { dg-do compile { target c++17 } }

template <int a> struct b { static constexpr int c = a; };
class d;
template <typename> struct e { typedef d f; };
template <typename g> using h = typename e<g>::f;
template <typename> constexpr bool i = b<true>::c;
class d {
public:
  using j = float;
};
template <typename> void k();
int main() { k<d>(); }
template <class l> l m;
template <class, class r> void n(r o) {
  [](int) {}(o(m<d>));
}
template <typename> void k() {
  n<int>([](auto inputs) {
    auto p(inputs);
    using s = h<decltype(p)>;
    s q;
    if constexpr (i<typename s::j>)
      [&] { return q; }();
    return 42;
  });
}
