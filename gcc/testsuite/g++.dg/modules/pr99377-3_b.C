// { dg-additional-options "-fmodules-ts" }
// { dg-module-cmi Foo:check }

export module Foo:check;
import "pr99377-3_a.H";

export inline bool Check (const Widget<int>& w)
{
  return w.Second ();
}
