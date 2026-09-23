// PR c++/126599
// { dg-do compile { target c++17 } }

struct S { int s, t, u[3][3]; };
using A = int[3][3];
template <auto M>
void foo (int);
void
bar ()
{
  foo <(int S::*)nullptr> ();	// { dg-error "no matching function for call to 'foo<\\\(\\\(int S::\\\*\\\)nullptr\\\)>\\\(\\\)'" }
  foo <&S::s> ();		// { dg-error "no matching function for call to 'foo<\\\&S::s>\\\(\\\)'" }
  foo <&S::t> ();		// { dg-error "no matching function for call to 'foo<\\\&S::t>\\\(\\\)'" }
  foo <(A S::*)nullptr> ();	// { dg-error "no matching function for call to 'foo<\\\(\\\(int \\\(S::\\\*\\\)\\\[3\\\]\\\[3\\\]\\\)nullptr\\\)>\\\(\\\)'" }
}
