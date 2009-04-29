// PR c++/34340
// { dg-options "-frepo" }
// { dg-final { cleanup-repo-files } }
// { dg-require-host-local "" }
// { dg-skip-if "dkms are not final links" { vxworks_kernel } }

struct A
{
  int a;
};

template <typename T> struct D
{
  static const A b;
};

template<typename T> const A D<T>::b = { 2 };

const A *x = &D<A>::b;

int
main ()
{
}
