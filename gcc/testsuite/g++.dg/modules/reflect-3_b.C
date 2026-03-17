// PR c++/124200
// { dg-do compile { target c++26 } }
// { dg-additional-options "-fmodules -freflection" }
// { dg-module-cmi M }

export module M;
import "reflect-3_a.H";

constexpr auto ctx = std::meta::access_context::unchecked();

export template <std::meta::info scope>
consteval std::size_t num_members() {
  constexpr auto result = members_of(scope, ctx).size();
  return result;
}

namespace ns {
  void foo();
  export void bar();
}
