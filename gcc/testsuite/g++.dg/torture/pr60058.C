/* { dg-do compile } */
/* { dg-require-visibility "" } */

typedef enum {} UErrorCode;
class A {
public:
  virtual A &m_fn1(A &, const A &, UErrorCode &) const;
  void m_fn2();
  A();
  A(int);
};
class __attribute__((visibility("hidden"))) B : public A {
public:
  B(A &p1) : norm2(p1), set(0) {}
  A &m_fn1(A &, const A &, UErrorCode &) const;
  A &norm2;
  const int &set;
};

UErrorCode a;
A c;
void fn1(A *p1) {
  A b;
  p1->m_fn1(b, 0, a).m_fn2();
}

void fn2() {
  B d(c);
  fn1(&d);
}
