// PR c++/120013
// { dg-additional-options "-fmodules" }
// { dg-module-cmi m }
// Same as partial-8_c.C but in the other order, to ensure
// that loading a partial spec over an instantiation works

export module m;
import :b;
import :a;
