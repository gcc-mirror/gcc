// { dg-do compile }
// { dg-options "" }

struct A { int x, z, y; };
struct B { int y, a, x; };
void f(A a, int);          // #1
void f(B b, ...);          // #2
void g(A a);               // #3	{ dg-message "candidate:" }
void g(B b);               // #4	{ dg-message "candidate:" }
void h() {
  f({.x = 1, .y = 2}, 0);  // OK; calls #1
			   // { dg-warning "extended initializer lists only available with" "" { target c++98_only } .-1 }
  f({.y = 2, .x = 1}, 0);  // error: selects #1
			   // { dg-error "designator order for field 'A::x' does not match declaration order in 'A'" "" { target *-*-* } .-1 }
			   // { dg-warning "extended initializer lists only available with" "" { target c++98_only } .-2 }
  f({.x = 1, .z = 2, .y = 3}, 0); // OK; calls #1
			   // { dg-warning "extended initializer lists only available with" "" { target c++98_only } .-1 }
  f({.y = 3, .a = 2, .x = 1}, 0); // OK; calls #2
			   // { dg-warning "extended initializer lists only available with" "" { target c++98_only } .-1 }
  g({.x = 1, .y = 2});     // error: ambiguous between #3 and #4
			   // { dg-error "is ambiguous" "" { target *-*-* } .-1 }
			   // { dg-warning "extended initializer lists only available with" "" { target c++98_only } .-2 }
}
