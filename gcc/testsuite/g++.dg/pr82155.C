/* { dg-do compile { target c++11 } } */
/* { dg-options "-g -O2" } */

template <typename a> struct b { a c; };
template <typename d> struct e { d *operator->(); };
template <typename d> class h {
public:
  typedef e<d> ag;
};
class i {
protected:
  i(int);
};
class j {
  virtual void k(int) = 0;

public:
  int f;
  void l() { k(f); }
};
struct m : i {
  int cn;
  m() : i(cn) {
    struct n : j {
      n() {}
      void k(int) {}
    };
  }
};
struct o {
  o() {
    for (h<b<b<j *>>>::ag g;;)
      g->c.c->l();
  }
};
void fn1() { o(); }
