// PR c++/88120
// { dg-do compile { target c++11 } }

typedef int a;
enum b : a;
class c {
  enum f { d };
  c(f);
  friend c operator&(c, c);
  typedef void (c::*e)();
  operator e();
};
class g {
  template <typename, typename> b h();
  struct k {
    c i;
  };
};
template <typename, typename> b g::h() {
  k j;
  &j || j.i &c::d;
  return b();
}
