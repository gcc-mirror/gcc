// PR c++/24996
// Bug: the cleanup for the A temporary was confusing the gimplifier
// because of the TRY_CATCH_EXPR for the exception object.

struct A { A(int); ~A(); };
struct B { B(A); ~B(); };
void foo(bool b)
{
  throw b ? B(1) : B(1);
}
