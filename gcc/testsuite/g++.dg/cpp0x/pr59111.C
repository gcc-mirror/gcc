// PR c++/59111
// { dg-do compile { target c++11 } }

auto& foo();	// { dg-error "type specifier without trailing return type" }
int i = foo();	// { dg-error "cannot convert" }
