// PR c++/56095

int *a(void) { return 0; }
typedef void voidfn(void);
template <voidfn* b> void z1(void) {}
template <voidfn& b> void z2(void) {}

int main()
{
  z1<(voidfn*)a>();		      // { dg-error "" }
  z1<reinterpret_cast<voidfn*>(a)>(); // { dg-error "" }
  z2<(voidfn&)a>();		      // { dg-error "" }
  z2<reinterpret_cast<voidfn&>(a)>(); // { dg-error "" }
}
