// PR c++/17775
// { dg-options "-frepo" }
// { dg-final { cleanup-repo-files } }

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
