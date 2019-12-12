// PR c++/90455
// { dg-do compile { target c++11 } }

struct B;
template <typename a> struct b {
  void operator()(a *) { sizeof(a); }
};
struct c {
  struct D {
    using d = B *;
  };

  using e = D::d;
  e f();
};
template <typename> class g {
  c h;
  using i = b<B>;
public:
  ~g() {
    auto j = h.f();
    k()(j);
  }
  i k();
};
struct l {
  g<int> m{};
};
