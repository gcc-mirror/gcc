// PR c++/83720
// { dg-lto-do assemble }

#pragma GCC diagnostic ignored "-Wreturn-type"

namespace b {
class h {
public:
  template <typename ae, typename af> h(ae af::*...) {
    [] {};
  }
};
class ai {};
template <typename> class c {
public:
  template <typename ag> void aj(const char *, ag f) { h(f, int()); }
};
}
template <typename> class al;
template <typename e> class i {
protected:
  static e g(const int) {  }
};
template <typename, typename> class j;
template <typename an, typename e, typename... ao>
class j<an(ao...), e> : i<e> {
  typedef i<e> ap;

public:
  static an aq(const int &ar, ao... as) { ap::g(ar)(as...); }
};
template <typename an, typename... ao> class al<an(ao...)> {
  template <typename, typename a> using ax = a;

public:
  template <typename e, typename = ax<int, void>, typename = ax<int, void>>
  al(e);
  using ay = an (*)(const int &, ao...);
  ay az;
};
template <typename an, typename... ao>
template <typename e, typename, typename>
al<an(ao...)>::al(e) {
  az = j<an(ao...), e>::aq;
}
class k {
public:
  k(al<void(b::ai)>);
} d([](b::ai) {
  struct be {
    virtual void f(){}
  };
  struct bf;
  b::c<bf>().aj("", &be::f);
});
