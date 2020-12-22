// { dg-additional-options -fmodules-ts }

export module Foo:A;
// { dg-module-cmi {Foo:A} }

namespace Bob
{
export int Random ();
}
