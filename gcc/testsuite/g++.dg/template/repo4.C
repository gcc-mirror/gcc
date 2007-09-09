// PR c++/17775
// { dg-options "-frepo" }
// { dg-final { cleanup-repo-files } }
// { dg-require-host-local "" }

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
