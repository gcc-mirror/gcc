// { dg-additional-options {-fmodules-ts -nostdinc} }
module;

import "macloc-2_a.H";

export module Foo;
// { dg-module-cmi Foo }

export inline int MACRO (fn) (int i)
{
  return frob (i);
}

export int (MACRO) (int i);
