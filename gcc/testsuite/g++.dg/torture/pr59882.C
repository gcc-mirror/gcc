/* { dg-do compile } */
class A;
class B {};
struct C {
  virtual void dispatch();
  int traversal_map_;
};
template <typename> class F : public virtual C {};

struct I : F<A>, F<int> {};
struct J : B, I {};
class D {};
struct L {
  L(D &, int &p2) : map_(p2) {}
  virtual void traverse(int &p1) {
    int &s = p1;
    names<L>(s, names_);
  }
  int &map_;
  J names_;
  template <typename> void names(int &, C &p2) { p2.dispatch(); }
};

struct G : D {
  G(D &, int &p2) : map_(p2) { L(*this, map_); }
  int &map_;
};

int a;
void fn1(D &p1) { G(p1, a); }
