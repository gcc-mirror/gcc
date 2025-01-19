// { dg-do compile { target c++11 } }

struct S {};
struct S [[gnu::deprecated]];	// { dg-warning "attribute ignored" }
// { dg-message "an attribute that appertains to a type-specifier is ignored" "" { target *-*-* } .-1 }
enum E {};
enum E [[gnu::deprecated]];	// { dg-warning "attribute ignored" }
// { dg-message "an attribute that appertains to a type-specifier is ignored" "" { target *-*-* } .-1 }
