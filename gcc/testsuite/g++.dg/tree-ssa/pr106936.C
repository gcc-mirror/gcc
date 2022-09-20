// { dg-do compile } */
// { dg-options "-O2 -fno-tree-ccp -fno-tree-forwprop -fno-tree-fre" }

namespace testPointerToMemberMiscCasts2 {
struct B {
  int f;
};
struct L : public B { };
struct R : public B { };
struct D : public L, R { };
  int B::* pb = &B::f;
  int R::* pr = pb;
  int D::* pdr = pr;
}
