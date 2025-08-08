// PR c++/120824
// { dg-additional-options "-fmodules -Wno-global-module -std=c++20" }
// { dg-module-cmi M }

module;
enum { E };
enum { F };
export module M;
export using ::E;
export using ::F;
