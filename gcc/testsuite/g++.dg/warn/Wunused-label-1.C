// { dg-do compile }
// { dg-options "-Wunused-label" }

extern void f9();

void
f1()
{
 l1: f9();				// { dg-warning "not used" }
 l3: ; f9();				// { dg-warning "not used" }
 l4: __attribute__ ((unused)) ; f9();
}

void
f2()
{
 label: __attribute ((unused)) ;
}

void
f3()
{
  // The next line would be OK in C but is a syntax error in C++.
 l2: __attribute__ ((unused)) f9();	// { dg-warning "ignored" }
		// We still get an unused label warning--this is
		// optional and can be removed if it ever changes.
		// { dg-warning "not used" "expected" { target *-*-* } 24 }
}
