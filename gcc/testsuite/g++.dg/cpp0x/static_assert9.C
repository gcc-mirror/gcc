// PR c++/58837
// { dg-require-effective-target c++11 }

void f();
static_assert(f, "");
struct A {};
static_assert(A::~A, "");      // { dg-error "non-static member function" }
