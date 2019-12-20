// PR c++/92648
// { dg-do compile { target c++11 } }

int a [[gnu::no_such_attribute(![!(!)!]!,;;)]];	// { dg-warning "ignored" }
int b [[no_such_namespace::nonexisting_attribute(linear(c, d : 2), reduction(*:e), linear(uval (f)))]];	// { dg-warning "ignored" }
int c [[gnu::nonexisting_attribute()]];	// { dg-warning "ignored" }
int d [[gnu::another_nonexistent_attr(1,"abcd",g+6)]];	// { dg-warning "ignored" }
