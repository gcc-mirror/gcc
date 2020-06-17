/* { dg-do compile } */
/* { dg-options "-O3 -w -std=gnu++11" } */

class a {
public:
  a(char *);
};
class ad {
public:
  ad(a *);
};
class b {};
using ah = class ak {
  using al = char;

public:
  ak(b) : ak(0) {}
  ak an() { return ap & 1; }
  al ap;
  ak(al af) : ap(af) {}
};
struct at {
  ah au;
  int av;
  char aw;
  char ax;
};
class az {
public:
  struct ba {
    void bb(ak am) {
      ak bc = am.an();
      bb(bc);
    }
  };
  void bd(ak am) { be.bb(am); }
  ba be;
};
class bg {
public:
  int bh;
  at bi;
};
int bj;
int *bk;
int c;
class bl {
  bool bm();
  bg bn;
  az bo;
  int e;
  int bq;
};
class bs {
public:
  bs(int *, ah *, char *, char *, int);
};
template < typename bt > class bu : bs {
  using bv = typename bt::bv;

public:
  template < typename... bx >
  bu(a *by, int *bz, at body, bx...)
      : bs(bz, &body.au, &body.aw, &body.ax, body.av), ca(bx()...), cb(by),
        cc(by), cd(by), ce(by) {}
  void cf() {
    auto cg = ch();
    auto ci = *cj();
    ca.ck(this, cg, &ci);
  }
  bt ca;
  ad cb;
  ad cc;
  ad cd;
  ad ce;
  bv *cj();
  bv ch();
};
class cl {
public:
  using bv = struct {};
  cl(az *, int, int, int, int, a *, int, int **);
  void ck(bs *, bv, bv *) {
    b d;
    ak ci(d);
    bo.bd(ci);
  }
  az bo;
};
bool bl::bm() {
  a by("");
  bu< cl > cn(&by, &bj, bn.bi, &bo, c, bn.bh, e, 0, &by, bq, &bk);
  cn.cf();
}

