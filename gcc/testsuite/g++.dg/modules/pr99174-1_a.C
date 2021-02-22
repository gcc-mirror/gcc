// PR 99174 what if we import the same module twice (with no
// intervening header import)?
// { dg-additional-options -fmodules-ts }

export module Foo;
// { dg-module-cmi Foo }

export void Foo ();
