// PR c++/67825
// { dg-do compile { target concepts } }
// { dg-additional-options "-fconcepts-diagnostics-depth=2" }

template<typename T>
  requires requires (T t) { t.f; } // { dg-error "invalid use of non-static member function" }
void foo() { }

struct S
{
  int f();
};

void
bar()
{
  foo<S>(); // { dg-error "no match" }
}
