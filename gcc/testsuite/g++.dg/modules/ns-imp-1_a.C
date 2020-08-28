// { dg-additional-options -fmodules-ts }

export module Foo;
// { dg-module-cmi {Foo} }

namespace Bob
{
export int Random ();
}
