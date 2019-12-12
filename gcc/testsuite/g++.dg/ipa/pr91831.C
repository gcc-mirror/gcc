/* { dg-do compile } */
/* { dg-options "-O2 --param uninlined-thunk-insns=1000"  } */

struct A {
  virtual void m_fn1();
};
struct B {
  virtual void *m_fn2(int, int) = 0;
};
struct C : A, B {
  void *m_fn2(int, int) { return this; }
};
void *fn1(B &p1) { return p1.m_fn2(0, 0); }

int main() {
  C c;
  fn1(c);
  return 0;
}
