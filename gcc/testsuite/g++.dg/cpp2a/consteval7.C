// { dg-do compile }
// { dg-options "-std=c++2a" }

consteval int foo () { return 42; }
consteval auto bar () { return foo; }
constexpr auto a = bar ();	// { dg-error "immediate evaluation returns address of immediate function 'consteval int foo\\(\\)'" }
struct S { int b; int (*c) (); };
consteval S baz () { return { 5, foo }; }
consteval int qux () { S s = baz (); return s.b + s.c (); }
consteval int quux () { constexpr S s = baz (); return s.b + s.c (); }
constexpr auto d = baz ();	// { dg-error "immediate evaluation returns address of immediate function 'consteval int foo\\(\\)'" }
constexpr auto e = qux ();
constexpr auto f = quux ();
