// PR c++/126783
// { dg-additional-options "-fmodules -Wno-global-module" }
// { dg-module-cmi M }
module;
extern int const q;
inline constexpr int q = 1;
export module M;
