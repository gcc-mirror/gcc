// { dg-do compile { target c++20 } }

#include <compare>

int main()
{
  { true <=> 1; }		// { dg-error "bool" }
  { int a[2]; a <=> a; }	// { dg-error "2" }
  { -1 <=> 1U; }		// { dg-error "narrowing" }
  { enum A { a }; enum B { b }; a <=> b; } // { dg-error "A" }
}
