// { dg-do assemble }
// { dg-options "-O2 -fdump-tree-fre1-details -std=c++11 -Wno-return-type" }
typedef unsigned a;
enum b : a;
class c {
public:
  virtual a d();
};
using e = int;
class f;
class h {
public:
  f *operator->();
};
class i {
public:
  ~i() { j->d(); }
  c *j;
};
template <class g> class k : i {
public:
  k(g *);
};
class l;
class m {
  virtual b n(const e &, l **);
};
class o {
protected:
  h p;
};
class G {
  virtual b r(const e &, l **);
};
class l : G {};
class q {
public:
  q(l *);
  template <class t> void s(t);
};
class f : c {
  a d();
  virtual b r(e);

public:
  class L : public l, o, m {
    b r(const e &y, l **) { p->r(y); }
    b n(const e &, l **) { k<l> a = this; }
  };
};
c u;
void fn1() {
  c v;
  k<c> b(&u);
  q(new f::L).s(v);
}
/* Check that f::d appears as possible target.  */
/* { dg-final { scan-tree-dump "f::d" "fre1"  } } */
