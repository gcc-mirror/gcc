// { dg-additional-options "-fmodules-ts" }

export module Bar;
// { dg-module-cmi "Bar" }

import Foo;

export int frob (int, float);
