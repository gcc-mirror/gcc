// PR c++/35335
// { dg-do compile }
// { dg-options "" }
// { dg-bogus "not supported by" "" { target *-*-* } 0 }

struct A {};		// { dg-message "operator=|no known conversion" }

void
foo ()
{
  A a;
  a = ({ { 1; } });	// { dg-error "no match for" }
}
