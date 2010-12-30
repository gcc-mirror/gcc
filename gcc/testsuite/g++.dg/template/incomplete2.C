// PR c++/27427
// { dg-do compile }

struct A;

template<A&> void foo();	// { dg-message "note" }

A a;  // { dg-error "incomplete type" }

void bar()
{
  foo<a>();  // { dg-error "no matching function" }
  // { dg-message "candidate" "candidate note" { target *-*-* } 12 }
}
