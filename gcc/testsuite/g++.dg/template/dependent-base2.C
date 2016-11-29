// PR c++/72796
// { dg-do compile { target c++11 } }

struct a;
template <int> struct b { typedef a c; };
struct d {
  void e(int);
};
struct a : d {
  void e(int) = delete;
};
template <int f> struct g : b<f>::c {
  g(int) { this->d::e(0); }
};
struct h : g<0> {
  using i = g;
  h() : i(0) {}
};
