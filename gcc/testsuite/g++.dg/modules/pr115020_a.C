// PR c++/115020
// { dg-additional-options "-fmodules-ts -Wno-global-module" }
// { dg-module-cmi M:a }

module;
struct Check { static void assertion(); };
void Check::assertion() {}

module M:a;
Check c;
