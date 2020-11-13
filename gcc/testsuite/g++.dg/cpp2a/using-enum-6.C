// { dg-do compile { target c++2a } }

using enum void;		// { dg-error "non-enum" }
struct A {};			// { dg-message "declared here" }
using enum A;			// { dg-error "non-enum" }
