// { dg-lto-do assemble }
struct Foo { Foo(int); }; void func() { new Foo(0); }
