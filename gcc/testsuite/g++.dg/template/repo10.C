// PR c++/51910
// { dg-options -frepo }
// { dg-require-host-local "" }
// { dg-skip-if "dkms are not final links" { vxworks_kernel } }
// { dg-final cleanup-repo-files }
// { dg-warning "is deprecated and will be removed in a future release" "" { target *-*-* } 0 }

template<typename T>
struct Foo
{
  virtual ~Foo() { }
};

int main( int, char*[] )
{
  Foo<int> test;
}
