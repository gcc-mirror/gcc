// PR c++/71446
// { dg-do compile { target c++11 } }
// { dg-options "" }

struct S { int a, b, c, d, e; };
struct T { int a, b; };
void foo (S);
void bar (T);

void
baz ()
{
  foo ({.d = 5, 6, .b = 2, 3});	// { dg-error "designator order for field 'S::b' does not match declaration order in 'S'" }
				// { dg-error "either all initializer clauses should be designated or none of them should be" "" { target c++2a } .-1 }
  bar ({.b = 1, .a = 2});	// { dg-error "designator order for field 'T::a' does not match declaration order in 'T'" }
}
