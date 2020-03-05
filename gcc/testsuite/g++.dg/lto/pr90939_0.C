// PR ipa/90939
// { dg-lto-do link }
// { dg-lto-options { { -flto -O3 } } }


typedef char uint8_t;
template <class T> class A {
public:
  A(T *);
};
template <typename Derived, typename Base> const Derived &To(Base &p1) {
  return static_cast<const Derived &>(p1);
}
class H;
template <typename, typename Base> const H *To(Base *p1) {
  return p1 ? &To<H>(*p1) : nullptr;
}
enum TextDirection : uint8_t;
enum WritingMode : unsigned;
class B {
public:
  WritingMode m_fn1();
};
class C {
public:
  int &m_fn2();
};
class D { double d;};
class H : public D {};
class F {
public:
  F(C, A<const int>, B *, WritingMode, TextDirection);
};

class G {
public:
  C NGLayoutAlgorithm_node;
  B NGLayoutAlgorithm_space;
  TextDirection NGLayoutAlgorithm_direction;
  H NGLayoutAlgorithm_break_token;
  G(A<const int> p1) __attribute__((noinline))
    : break_token_(&NGLayoutAlgorithm_break_token),
        container_builder_(NGLayoutAlgorithm_node, p1, &NGLayoutAlgorithm_space,
                           NGLayoutAlgorithm_space.m_fn1(),
                           NGLayoutAlgorithm_direction) {}
  G(C p1, const H *) : G(&p1.m_fn2()) {}
  A<H> break_token_;
  F container_builder_;
};

class I : G {
public:
  I(const D *) __attribute__((noinline));
};
C a;
I::I(const D *p1) : G(a, To<H>(p1)) {}

D gd[10];

int main (int argc, char *argv[])
{
  I i(&(gd[argc%2]));
  return 0;
}
