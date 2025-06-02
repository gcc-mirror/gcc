// PR c++/120013
// { dg-additional-options "-fmodules" }
// { dg-module-cmi m }

export module m;
import :a;
import :b;
