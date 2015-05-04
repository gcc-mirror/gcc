// PR c++/58837
// { dg-require-effective-target c++11 }
// { dg-skip-if "" keeps_null_pointer_checks }
// { dg-options "-fdelete-null-pointer-checks" }

void f();
static_assert(f, "");
struct A {};
static_assert(A::~A, "");      // { dg-error "non-static member function" }
