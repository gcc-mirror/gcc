// { dg-lto-do assemble }
struct Foo { double x[3]; };
Foo func() { Foo f = { { 0, 0, 0 } }; return f; }
