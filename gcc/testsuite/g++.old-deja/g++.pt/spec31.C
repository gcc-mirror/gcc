// { dg-do assemble  }
template <> struct A {};	// { dg-error "" } not a specialization
template <> void f ();		// { dg-error "" } not a specialization
