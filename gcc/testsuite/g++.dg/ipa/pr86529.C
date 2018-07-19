// { dg-do compile }
// { dg-options "-fipa-pure-const -std=c++11" }

namespace std {
class type_info {
public:
  bool operator==(type_info);
};
class c {
public:
  c(int) {}
  type_info b;
  virtual void *d() {
    if (b == typeid(int))
      return e();
    return nullptr;
  }
  int *e() noexcept;
};
class h {
public:
  template <typename g, typename f> h(g, f j) { new c(j); }
};
class k {
protected:
  int n;
  k() : i(0, n) {}
  h i;
};
class F : k {
  public:
  F(int, int) {}
  template <typename, typename f, typename...> friend F l(const f &);
};
template <typename, typename f, typename...> F l(const f &p1) { F x(int(), p1); return x; }
template <typename> F m() { l<int>(int()); return F(0, 0); }
class D {
  F p;

public:
  D() : p(m<int>()) {}
};
} // namespace std
std::D a;
