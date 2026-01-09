// PR c++/122609
// PR c++/101140
// { dg-additional-options "-fmodules -Wno-global-module -fdump-lang-module" }
// { dg-module-cmi M }

module;

using size_t = decltype(sizeof(0));

namespace std {
  template <typename T> struct tuple_size;
  template <size_t I, typename T> struct tuple_element;
}

struct G {};
template <> struct std::tuple_size<G> { static constexpr size_t value = 1; };
template <> struct std::tuple_element<0, G> { using type = int; };
template <size_t I> int get(G) { return 123; }

export module M;

export using ::G;
export template <typename T> void use_gmf(T t) {
  // This should make std::tuple_size and std::tuple_element decl-reachable;
  // additionally, this should make ::get reachable via ADL.
  auto [x] = t;
  // { dg-final { scan-lang-dump "Bindings '::std::tuple_size'" module { xfail *-*-* } } }
  // { dg-final { scan-lang-dump "Bindings '::std::tuple_element'" module { xfail *-*-* } } }
  // { dg-final { scan-lang-dump "Bindings '::get'" module { xfail *-*-* } } }
}

export template <typename T> void use_future_decl(T t) {
  ::new (t) int;
}

export struct F {};
void* operator new(size_t, F);  // not exported
