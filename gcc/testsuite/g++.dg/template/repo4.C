// PR c++/17775
// { dg-options "-frepo" }
// { dg-final { cleanup-repo-files } }
// { dg-require-host-local "" }
// { dg-skip-if "dkms are not final links" { vxworks_kernel } }
// { dg-warning "is deprecated and will be removed in a future release" "" { target *-*-* } 0 }

namespace { 
  struct Foo {}; 
} 
 
template <typename Tp> 
void foo(Tp) {} 
 
int 
main() 
{ 
  foo(Foo()); 
} 
