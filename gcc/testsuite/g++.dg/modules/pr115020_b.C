// PR c++/115020
// { dg-additional-options "-fmodules-ts -Wno-global-module" }
// { dg-module-cmi M }

module;
struct Check { static void assertion(); };

export module M;
import :a;
void foo() { Check::assertion(); }
