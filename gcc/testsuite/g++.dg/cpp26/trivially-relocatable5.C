// P2786R13 - C++26 Trivial Relocatability
// { dg-do compile { target c++11 } }
// { dg-additional-options "-Wc++26-compat" }

struct A __trivially_relocatable_if_eligible { A (const A &&); };
struct B __replaceable_if_eligible { B (const B &&); B &operator= (B &&); };
struct C __replaceable_if_eligible __final __trivially_relocatable_if_eligible { C (const C &&); C &operator= (C &&); };
#if __cpp_trivial_relocatability >= 202502L
struct D trivially_relocatable_if_eligible { D (const D &&); };
struct E replaceable_if_eligible { E (const E &&); E &operator= (E &&); };
struct F trivially_relocatable_if_eligible replaceable_if_eligible final { F (const F &&); F &operator= (F &&); };
#else
struct D trivially_relocatable_if_eligible {};		// { dg-warning "identifier 'trivially_relocatable_if_eligible' is a conditional keyword in" "" { target c++23_down } }
// { dg-error "variable 'D trivially_relocatable_if_eligible' has initializer but incomplete type" "" { target c++23_down } .-1 }
struct E replaceable_if_eligible {};			// { dg-warning "identifier 'replaceable_if_eligible' is a conditional keyword in" "" { target c++23_down } }
// { dg-error "variable 'E replaceable_if_eligible' has initializer but incomplete type" "" { target c++23_down } .-1 }
struct F trivially_relocatable_if_eligible replaceable_if_eligible {};		// { dg-warning "identifier 'trivially_relocatable_if_eligible' is a conditional keyword in" "" { target c++23_down } }
// { dg-error "expected initializer before 'replaceable_if_eligible'" "" { target c++23_down } .-1 }
#endif
#if __cplusplus <= 202302L
struct G {};
struct G trivially_relocatable_if_eligible {};		// { dg-warning "identifier 'trivially_relocatable_if_eligible' is a conditional keyword in" "" { target c++23_down } }
struct H {};
struct H replaceable_if_eligible {};			// { dg-warning "identifier 'replaceable_if_eligible' is a conditional keyword in" "" { target c++23_down } }
struct I {};
struct I trivially_relocatable_if_eligible replaceable_if_eligible {};		// { dg-warning "identifier 'trivially_relocatable_if_eligible' is a conditional keyword in" "" { target c++23_down } }
#endif							// { dg-error "expected initializer before 'replaceable_if_eligible'" "" { target c++23_down } .-1 }
struct J {};
struct J __trivially_relocatable_if_eligible {};	// { dg-error "redefinition of 'struct J'" }
struct K {};
struct K __replaceable_if_eligible {};			// { dg-error "redefinition of 'struct K'" }
struct L {};
struct L __trivially_relocatable_if_eligible __replaceable_if_eligible {};	// { dg-error "redefinition of 'struct L'" }
struct M __trivially_relocatable_if_eligible __trivially_relocatable_if_eligible {}; // { dg-error "duplicate '__trivially_relocatable_if_eligible' specifier" }
struct N __replaceable_if_eligible __replaceable_if_eligible {};		// { dg-error "duplicate '__replaceable_if_eligible' specifier" }
struct O __trivially_relocatable_if_eligible __replaceable_if_eligible __replaceable_if_eligible __trivially_relocatable_if_eligible final final {};
// { dg-error "duplicate '__replaceable_if_eligible' specifier" "" { target *-*-* } .-1 }
// { dg-error "duplicate '__trivially_relocatable_if_eligible' specifier" "" { target *-*-* } .-2 }
// { dg-error "duplicate 'final' specifier" "" { target *-*-* } .-3 }
#if __cpp_trivial_relocatability >= 202502L
struct P trivially_relocatable_if_eligible trivially_relocatable_if_eligible {}; // { dg-error "duplicate 'trivially_relocatable_if_eligible' specifier" "" { target c++26 } }
struct Q replaceable_if_eligible replaceable_if_eligible {};			// { dg-error "duplicate 'replaceable_if_eligible' specifier" "" { target c++26 } }
struct R trivially_relocatable_if_eligible replaceable_if_eligible replaceable_if_eligible trivially_relocatable_if_eligible final final {};
// { dg-error "duplicate 'replaceable_if_eligible' specifier" "" { target c++26 } .-1 }
// { dg-error "duplicate 'trivially_relocatable_if_eligible' specifier" "" { target c++26 } .-2 }
// { dg-error "duplicate 'final' specifier" "" { target c++26 } .-3 }
struct S trivially_relocatable_if_eligible __trivially_relocatable_if_eligible {}; // { dg-error "duplicate '__trivially_relocatable_if_eligible' specifier" "" { target c++26 } }
struct T replaceable_if_eligible __replaceable_if_eligible {};			// { dg-error "duplicate '__replaceable_if_eligible' specifier" "" { target c++26 } }
struct U trivially_relocatable_if_eligible replaceable_if_eligible __replaceable_if_eligible __trivially_relocatable_if_eligible final __final {};
// { dg-error "duplicate '__replaceable_if_eligible' specifier" "" { target c++26 } .-1 }
// { dg-error "duplicate '__trivially_relocatable_if_eligible' specifier" "" { target c++26 } .-2 }
// { dg-error "duplicate '__final' specifier" "" { target c++26 } .-3 }
struct V __trivially_relocatable_if_eligible trivially_relocatable_if_eligible {}; // { dg-error "duplicate 'trivially_relocatable_if_eligible' specifier" "" { target c++26 } }
struct W __replaceable_if_eligible replaceable_if_eligible {};			// { dg-error "duplicate 'replaceable_if_eligible' specifier" "" { target c++26 } }
struct X __trivially_relocatable_if_eligible __replaceable_if_eligible replaceable_if_eligible trivially_relocatable_if_eligible __final final {};
// { dg-error "duplicate 'replaceable_if_eligible' specifier" "" { target c++26 } .-1 }
// { dg-error "duplicate 'trivially_relocatable_if_eligible' specifier" "" { target c++26 } .-2 }
// { dg-error "duplicate 'final' specifier" "" { target c++26 } .-3 }
#else
struct Y {};
Y foo ();
struct Y trivially_relocatable_if_eligible = foo ();    // { dg-warning "identifier 'trivially_relocatable_if_eligible' is a conditional keyword in" "" { target c++23_down } }
struct Z {};
Z bar ();
struct Z replaceable_if_eligible = bar ();		// { dg-warning "identifier 'replaceable_if_eligible' is a conditional keyword in" "" { target c++23_down } }
#endif

static_assert (__builtin_is_trivially_relocatable (A), "");
static_assert (__builtin_is_replaceable (B), "");
static_assert (__builtin_is_trivially_relocatable (C), "");
static_assert (__builtin_is_replaceable (C), "");
#if __cpp_trivial_relocatability >= 202502L
static_assert (__builtin_is_trivially_relocatable (D), "");
static_assert (__builtin_is_replaceable (E), "");
static_assert (__builtin_is_trivially_relocatable (F), "");
static_assert (__builtin_is_replaceable (F), "");
#endif
