// PR c++/95552
// Test for VLA and cloned constructor.
// { dg-additional-options -Wno-vla }
// { dg-require-effective-target alloca }

struct VB { };
struct ViewDom: virtual VB
{
  ViewDom(int i) { char (*a)[i]; }
};
void element( )
{
  ViewDom a(2);
}
