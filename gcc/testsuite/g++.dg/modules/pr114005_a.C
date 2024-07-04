// { dg-additional-options "-fmodules-ts" }
// { dg-module-cmi M }

module;
#include <initializer_list>

export module M;
export constexpr std::initializer_list<int> foo{ 1, 2, 3 };
