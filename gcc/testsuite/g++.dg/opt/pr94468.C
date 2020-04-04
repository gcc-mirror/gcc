// PR rtl-optimization/94468
// { dg-do compile { target c++11 } }
// { dg-options "-O2" }
// { dg-additional-options "-fPIC" { target fpic } }

bool a();
enum b {};
class c;
template <typename> struct d;
template <class e, typename g, typename... h> struct d<g (e::*)(h...)> {
  typedef e i;
};
struct j { j(void(int, j *, c *, void **, bool *)) {} };
template <typename l> struct m : public j {
  l ab;
  static void ac(int, j *, c *, void **, bool *);
  m(l f) : j(ac), ab(f) {}
};
b ad;
struct c {
  template <typename n, typename o>
  void ae(typename d<n>::i *p, n af, typename d<o>::i *ag, o ah) {
    ai(p, &af, ag, &ah, new m<o>(ah), ad, &d<n>::i::aj);
  }
  void ai(c *, void *, c *, void *, j *, b, int *);
};
struct r : public c { static int aj; void t(); };
struct al : public c {
  static int aj;
  void am();
  void ao();
  void ap();
};
struct aq { aq(const int &, const int & = int()); };
struct ar : public c { ~ar(); };
struct as : public ar {
  as();
  void at();
  void au();
  void av();
};
struct u : public c { void ax(); };
struct ay { int az(); };
struct ba : public c { static int aj; void bb(); };
struct bc : public al { bc() { if (a()) am(); } };
as::as() {
  al *bd = new bc;
  ae(bd, &al::ao, this, &as::au);
  ae(bd, &al::ap, this, &as::av);
  r be;
  u bf;
  ae(&be, &r::t, &bf, &u::ax);
  c bg = *bd;
  ae(static_cast<ba *>(&bg), &ba::bb, this, &as::at);
  ay bh;
  aq am(bh.az());
}
