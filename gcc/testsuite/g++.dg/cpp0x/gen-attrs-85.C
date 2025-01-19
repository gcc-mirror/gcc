// { dg-do compile { target c++11 } }

struct S {};
struct S [[gnu::deprecated]] s;	// { dg-warning "attribute ignored" }
// { dg-message "an attribute that appertains to a type-specifier is ignored" "" { target *-*-* } .-1 }
enum E {} [[gnu::deprecated]];	// { dg-warning "attribute ignored in declaration of 'enum E'" }
// { dg-message "attribute for 'enum E' must follow the 'enum' keyword" "" { target *-*-* } .-1 }
