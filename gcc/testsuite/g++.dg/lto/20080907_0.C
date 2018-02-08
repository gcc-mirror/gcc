// { dg-lto-do assemble }
// { dg-lto-options "-Wno-return-type" }

struct Foo { void func (); }; Foo & bar () { } struct Baz { Baz (Baz &); };
Baz dummy() { bar().func(); }
