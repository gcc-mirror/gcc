/* { dg-do compile } */
/* { dg-options "-O3 --param max-early-inliner-iterations=5" } */

char *s;
namespace a {
template <class ae> class af {
public:
  af(ae);
};
typedef af<char *> b;
namespace ag {
class ah {
public:
  void ai(b aj) { c(aj); }
  virtual void c(b);
};
class d : public ah {
  void c(b);
};
class e {
  void f(bool);
  void ai(b aj) { g.ai(aj); }
  d g;
};
void d::c(b) {}
void e::f(bool) { ai(s); }
}
}
