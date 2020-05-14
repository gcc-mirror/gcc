// { dg-additional-options -fmodules-ts }
module;

import "gmf-2_a.H";

export module Foo;
// { dg-module-cmi Foo }

export inline int MACRO (fn) (int i)
{
  return frob (i);
}

export int (MACRO) (int i);
