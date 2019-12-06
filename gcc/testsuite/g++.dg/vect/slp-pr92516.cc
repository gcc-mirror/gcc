// { dg-do compile }
// { dg-require-effective-target c++14 }

class a {
public:
  typedef int b;
  operator b();
};
class c {
public:
  constexpr int m_fn1() const;
  constexpr int d() const;
  int e;
  int f;
};
constexpr int c::m_fn1() const { return e; }
constexpr int c::d() const { return f; }
class g {
public:
  g();
  constexpr void i(const c &) noexcept;
  int j;
  int k;
  int l;
  int m;
};
constexpr void g::i(const c &n) noexcept {
  int v = l - j, h = m - k;
  j = n.m_fn1() - v / 2;
  k = n.d() - h / 2;
  l = j + v;
  m = k + h;
}
class o {
  void m_fn4() const;
  a p;
} r;
void o::m_fn4() const {
  g q;
  c t;
  q.i(t);
  r.p || 0;
}
