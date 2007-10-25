// PR c++/33843

struct A {};

void foo(A* p())
{
  p->A::~A(); // { dg-error "A::~A" }
}
