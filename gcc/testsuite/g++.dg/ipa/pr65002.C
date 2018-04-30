/* PR tree-optimization/65002 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

namespace fastmath {
  template <typename T> float floor(const T &) __attribute__((const));
  template <typename T> float floor(const T &p1) { return p1; }
}
using fastmath::floor;
class A {
public:
  A(int, int);
  virtual void m_fn1(float) const;
};
class B : A {
public:
  B(int, int p2) : A(entity, p2) {}
  void m_fn1(float p1) const { long b(floor(p1)); }
  int entity;
};

int a;
void Convert() {
  if (int *c = 0)
    B(*c, a);
}
