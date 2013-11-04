// { dg-options -std=c++11 }
// PR c++/33837
void foo()
{
  __decltype (A::foo()); // { dg-error "was not declared|expected" }
  __decltype (B); // { dg-error "was not declared" }
}
