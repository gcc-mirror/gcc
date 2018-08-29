// { dg-lto-do link }
// { dg-require-effective-target shared }
// { dg-require-effective-target fpic }
// { dg-lto-options {{-O2 -fPIC -shared -flto}} }

int a;
void b(...);
void c(int);
enum { d, e, f, g, h, i, j, k };
class l {
public:
  int ac;
  bool m;
  l(char *);
  int n();
};
struct o {
  int ad;
  o(int p = 0) : ad(p) {}
};
class C : public l {
public:
  char q;
  C(o) : l(&q) { m |= ac & a ?: 9; }
};
class r : C {
public:
  char s;
  r(o p, char) : C(p) {
    if (n()) {
      b(a, s, "");
      c(5);
    }
  }
};
class t : C {
public:
  t(int) : C(d) {}
};
r ag('b', 0), ah(o(), 0), ai(e, 0), aj(f, 0), ak(g, 0), al(h, 0), am(k, 0),
    an(i, 0), ao(j, 0);
t ap(0);
