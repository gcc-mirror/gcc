// PR c++/34178
// { dg-options "-frepo" }
// { dg-final { cleanup-repo-files } }
// { dg-require-host-local "" }
// { dg-skip-if "dkms are not final links" { vxworks_kernel } }
// { dg-warning "is deprecated and will be removed in a future release" "" { target *-*-* } 0 }

template<typename T>
class A
{
private:
  static const int x;
  static int y;

public:
  int getX () { return x + y; }
};

template<typename T> const int A<T>::x = 0;
template<typename T> int A<T>::y = 0;

int
main ()
{
  A<int> a;
  return a.getX();
}
