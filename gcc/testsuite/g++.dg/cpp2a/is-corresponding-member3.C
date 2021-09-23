// P0466R5
// { dg-do compile { target c++20 } }

struct A { int a; };
struct B;

bool a = __builtin_is_corresponding_member ();			// { dg-error "needs two arguments" }
bool b = __builtin_is_corresponding_member (&A::a);		// { dg-error "needs two arguments" }
bool c = __builtin_is_corresponding_member (&A::a, &A::a, &A::a);	// { dg-error "needs two arguments" }
bool d = __builtin_is_corresponding_member (&A::a, 1);			// { dg-error "argument is not pointer to member" }
bool e = __builtin_is_corresponding_member (1.0, &A::a);		// { dg-error "argument is not pointer to member" }
bool f = __builtin_is_corresponding_member (1, A{});		// { dg-error "argument is not pointer to member" }
bool g = __builtin_is_corresponding_member (&A::a, (int B::*) nullptr);	// { dg-error "invalid use of incomplete type" }
bool h = __builtin_is_corresponding_member ((int B::*) nullptr, &A::a);	// { dg-error "invalid use of incomplete type" }
