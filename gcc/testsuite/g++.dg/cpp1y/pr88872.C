// PR c++/88872
// { dg-do compile { target c++14 } }

struct a {
  template <typename b> constexpr a(b) : c() {}
  int c;
};
void d();
template <char...> constexpr a operator ""_n() { return d; }
struct e;
struct f {
  e operator[](int);
};
struct g {
  void h();
  f i;
};
template <typename> struct j {
  void k() { [](auto) { constexpr auto l = 2_n; }(keywords); }
  int keywords;
};
using m = j<int>;
class e : public m {};
void g::h() { i[0].k(); }
