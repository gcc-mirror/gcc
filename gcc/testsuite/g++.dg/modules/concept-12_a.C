// PR c++/122789
// { dg-additional-options "-fmodules -fconcepts" }
// { dg-module-cmi M }

export module M;
template <typename T> constexpr bool b = requires(int, typename T::U x) { x; };
