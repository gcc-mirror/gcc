// PR c++/38701, 38702
// { dg-options "-std=c++11" }

void foo() = default;		// { dg-error "cannot be defaulted" }
namespace
{
  void bar() = default;		// { dg-error "cannot be defaulted" }
}

enum E { e };

E& operator |= (E&, const E&) = default; // { dg-error "cannot be defaulted" }
