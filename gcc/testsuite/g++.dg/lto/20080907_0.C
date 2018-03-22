// { dg-lto-do assemble }

/* "WARNING: lto.exp does not support dg-additional-options" */
#pragma GCC diagnostic ignored "-Wreturn-type"

struct Foo { void func (); }; Foo & bar () { } struct Baz { Baz (Baz &); };
Baz dummy() { bar().func(); }
