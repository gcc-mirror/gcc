// { dg-additional-options -fmodules-ts }
export module Foo;
// { dg-module-cmi Foo }
import "pr99377_a.H";

export inline bool Check (const Widget<int>& w)
{
  return w.Second ();
}

