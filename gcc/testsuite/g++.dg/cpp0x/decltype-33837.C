// { dg-options -std=c++0x }
// PR c++/33837
void foo()
{
  __decltype (A::foo()); // { dg-error "was not declared|expected initializer" }
  __decltype (B); // { dg-error "was not declared" }
}
