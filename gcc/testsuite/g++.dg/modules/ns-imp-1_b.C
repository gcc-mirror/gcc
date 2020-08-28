// { dg-additional-options -fmodules-ts }

export module Bar;
// { dg-module-cmi {Bar} }

export import Foo;

namespace Bob
{
export int Quux ();
}
