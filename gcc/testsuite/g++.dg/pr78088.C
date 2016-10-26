// PR tree-optimization/78088
// { dg-do compile }
// { dg-options "-O3 -fsplit-loops" }
class A {
public:
  int m_fn1();
};
struct B : A {
  void m_fn2();
};
void B::m_fn2() {
  long a;
  int b, c;
  for (;;) {
    c = 0;
    for (; c < a; ++c, ++b)
      b > 0 ? m_fn1() : 0;
  }
}
