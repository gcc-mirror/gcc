// PR c++/87554
// { dg-options "-O" }

template < class a > class b {
  static void c(a);
  static a &create() { c(instance); return mya; }

  static a mya;

public:
  static a d() { create(); return a(); }
  static a &instance;
};
template < class a > a &b< a >::instance = create();
class e;
class f {
public:
  void operator()(int g) { h(g); }
  template < class a > void h(a i) { p(j, i); }
  e *j;
};
class e : public f {
public:
  e(int);
};
struct k {
  int l;
};
template < class m, class a > void p(m, a) { b< k >::d(); }
extern template class b< k >;
int n;
int o;
void test() {
  e out(o);
  out(n);
}
