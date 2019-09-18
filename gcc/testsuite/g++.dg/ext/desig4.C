// PR c++/51458
// { dg-options "" }

char g[] = { [7] = "abcd" };	     // { dg-error "15:designator .7." }
int a = { .foo = 6 };		     // { dg-error "designator" }
int b = { [0] = 1 };		     // { dg-error "12:designator .0." }
_Complex float c = { .foo = 0,  1 }; // { dg-error "designator" }
				     // { dg-error "either all initializer clauses should be designated or none of them should be" "" { target c++2a } .-1 }
				     // { dg-error "cannot convert" "" { target *-*-* } .-2 }
_Complex float d = { [0] = 0,  1 };  // { dg-error "23:designator .0." }
				     // { dg-error "either all initializer clauses should be designated or none of them should be" "" { target c++2a } .-1 }
				     // { dg-error "cannot convert" "" { target *-*-* } .-2 }
_Complex float e = { 0, .foo = 1 };  // { dg-error "designator" }
				     // { dg-error "either all initializer clauses should be designated or none of them should be" "" { target c++2a } .-1 }
				     // { dg-error "cannot convert" "" { target *-*-* } .-2 }
_Complex float f = { 0, [0] = 1 };   // { dg-error "26:designator .0." }
				     // { dg-error "either all initializer clauses should be designated or none of them should be" "" { target c++2a } .-1 }
				     // { dg-error "cannot convert" "" { target *-*-* } .-2 }
