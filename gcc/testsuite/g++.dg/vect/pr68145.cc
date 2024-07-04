/* { dg-do compile } */
/* { dg-additional-options "-fno-tree-pta -Wno-return-type" } */

struct A {
  bool operator()(int p1, int p2) { return p1 && p2; }
};
class B {
public:
  bool *cbegin();
  bool *cend();
};
template <class T> void operator&&(B p1, T p2) {
  B a;
  arrayContTransform(p1, p2, a, A());
}

template <typename _InputIterator1, typename T, typename _OutputIterator,
          typename _BinaryOperation>
void myrtransform(_InputIterator1 p1, _OutputIterator p2, T p3,
                  _BinaryOperation p4) {
  _InputIterator1 b;
  for (; b != p1; ++b, ++p2)
    *p2 = p4(*b, p3);
}

template <typename L, typename R, typename RES, typename BinaryOperator>
void arrayContTransform(L p1, R p2, RES p3, BinaryOperator p4) {
  myrtransform(p1.cend(), p3.cbegin(), p2, p4);
}

class C {
public:
  B getArrayBool();
};
class D {
  B getArrayBool(const int &);
  C lnode_p;
};
bool c;
B D::getArrayBool(const int &) { lnode_p.getArrayBool() && c; }

// { dg-final { scan-tree-dump "vectorized 1 loops" "vect" { target { i?86-*-* x86_64-*-* } } } }
