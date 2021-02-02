// { dg-additional-options -fmodules-ts }

export module Foo;
// { dg-module-cmi {Foo} }

export import :A;
export import :B;

namespace Bob
{
export void Widget ()
{
  Random ();
  Quux ();
}
}
