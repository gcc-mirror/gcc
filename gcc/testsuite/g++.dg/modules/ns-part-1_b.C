// { dg-additional-options -fmodules-ts }

export module Foo:B;
// { dg-module-cmi {Foo:B} }

export import :A;

namespace Bob
{
export int Quux ();
}
