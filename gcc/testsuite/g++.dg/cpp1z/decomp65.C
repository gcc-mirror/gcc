// PR c++/121442
// { dg-do compile { target c++11 } }
// { dg-options "" }

struct S { int a, b, c, d, e; };

void
foo ()
{
  auto [a, b, b, b, c ] = S {};		// { dg-warning "structured bindings only available with" "" { target c++14_down } }
					// { dg-error "redeclaration of 'auto b'" "" { target *-*-* } .-1 }
					// { dg-message "'auto b' previously declared here" "" { target *-*-* } .-2 }
}
