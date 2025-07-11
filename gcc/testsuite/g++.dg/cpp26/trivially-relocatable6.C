// P2786R13 - C++26 Trivial Relocatability
// { dg-do compile { target c++98_only } }
// { dg-additional-options "-Wc++26-compat" }

struct A __trivially_relocatable_if_eligible {};
struct B __replaceable_if_eligible {};
struct C __replaceable_if_eligible __final __trivially_relocatable_if_eligible {};
struct D trivially_relocatable_if_eligible {};		// { dg-warning "identifier 'trivially_relocatable_if_eligible' is a conditional keyword in" }
// { dg-error "variable 'D trivially_relocatable_if_eligible' has initializer but incomplete type" "" { target *-*-* } .-1 }
// { dg-error "extended initializer lists only available with" "" { target *-*-* } .-2 }
struct E replaceable_if_eligible {};			// { dg-warning "identifier 'replaceable_if_eligible' is a conditional keyword in" }
// { dg-error "variable 'E replaceable_if_eligible' has initializer but incomplete type" "" { target *-*-* } .-1 }
// { dg-error "extended initializer lists only available with" "" { target *-*-* } .-2 }
struct F trivially_relocatable_if_eligible replaceable_if_eligible {};		// { dg-warning "identifier 'trivially_relocatable_if_eligible' is a conditional keyword in" }
// { dg-error "expected initializer before 'replaceable_if_eligible'" "" { target *-*-* } .-1 }
struct G {};
struct G trivially_relocatable_if_eligible {};		// { dg-warning "identifier 'trivially_relocatable_if_eligible' is a conditional keyword in" }
// { dg-error "extended initializer lists only available with" "" { target *-*-* } .-1 }
struct H {};
struct H replaceable_if_eligible {};			// { dg-warning "identifier 'replaceable_if_eligible' is a conditional keyword in" }
// { dg-error "extended initializer lists only available with" "" { target *-*-* } .-1 }
struct I {};
struct I trivially_relocatable_if_eligible replaceable_if_eligible {};		// { dg-warning "identifier 'trivially_relocatable_if_eligible' is a conditional keyword in" }
// { dg-error "expected initializer before 'replaceable_if_eligible'" "" { target *-*-* } .-1 }
struct J {};
J foo ();
struct J trivially_relocatable_if_eligible = foo ();	// { dg-warning "identifier 'trivially_relocatable_if_eligible' is a conditional keyword in" }
struct K {};
K bar ();
struct K replaceable_if_eligible = bar ();		// { dg-warning "identifier 'replaceable_if_eligible' is a conditional keyword in" }
