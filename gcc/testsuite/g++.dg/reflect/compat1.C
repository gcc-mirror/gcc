// C++26 P2996R13 - Reflection for C++26
// [diff.cpp23.dcl.dcl] tests
// { dg-do compile { target c++11 } }
// { dg-options "" }

struct C { int operator ^ (int); };
int operator ^ (int (C::*p) (int), C);
int i = &C::operator ^^ C {};	// { dg-error "expected type-specifier before '\\\^\\\^' token" "" { target c++26 } }

struct [[using CC:]] C;		// { dg-warning "attribute using prefix only available with" "" { target c++14_down } }
				// { dg-error "expected '\\\]' before 'CC'" "" { target c++26 } .-1 }
				// { dg-error "expected identifier before ';' token" "" { target c++26 } .-2 }
struct [[using DD: ]] D;	// { dg-warning "attribute using prefix only available with" "" { target c++14_down } }
