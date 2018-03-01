// { dg-do compile { target c++11 } }
// PR c++/33837
void foo()
{
  __decltype (A::foo()); // { dg-error "A" }
  __decltype (B); // { dg-error "B" }
}
