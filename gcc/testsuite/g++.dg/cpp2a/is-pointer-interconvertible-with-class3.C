// P0466R5
// { dg-do compile { target c++20 } }

struct A { int a; };
struct B;

bool a = __builtin_is_pointer_interconvertible_with_class ();			// { dg-error "needs a single argument" }
bool b = __builtin_is_pointer_interconvertible_with_class (&A::a, &A::a);	// { dg-error "needs a single argument" }
bool c = __builtin_is_pointer_interconvertible_with_class (1);			// { dg-error "argument is not pointer to member" }
bool d = __builtin_is_pointer_interconvertible_with_class (1.0);		// { dg-error "argument is not pointer to member" }
bool e = __builtin_is_pointer_interconvertible_with_class ((int B::*) nullptr);	// { dg-error "invalid use of incomplete type" }
