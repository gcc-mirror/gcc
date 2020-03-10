// { dg-do compile }
// { dg-options "-std=c++2a" }

consteval int foo () { return 0; }
int bar (int (*) ());
auto sz = sizeof (bar (foo));	// { dg-bogus "taking address of an immediate function" }
decltype (bar (foo)) baz;	// { dg-bogus "taking address of an immediate function" }
