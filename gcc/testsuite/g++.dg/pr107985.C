/* { dg-do compile } */
/* { dg-options "-O1 -ftree-vrp -fno-tree-ccp -fno-tree-forwprop -fno-tree-fre" } */

struct B {
  int f;
};

struct D : public B {
};

void foo() {
  D d;
  d.f = 7;

  int B::* pfb = &B::f;
  int D::* pfd = pfb;
  int v = d.*pfd;
}
