// { dg-additional-options "-fmodules -Wno-global-module -fdump-lang-module" }
// { dg-module-cmi M }

// Check we implement [module.global.frag] p2.3:
// A declaration D is decl-reachable from a declaration S in the same
// translation unit if: [...] S contains a dependent call E ([temp.dep]) and
// D is found by any name lookup performed for an expression synthesized from
// E by replacing each type-dependent argument or operand with a value of a
// placeholder type with no associated namespaces or entities.

module;
namespace Q {
  struct X {};
  void g_impl(X, X);
  void operator-(X, X);
  void go_partial(X, int);
  void operator/(X, int);
}
namespace P {
  struct X {};
}
#if __cpp_impl_three_way_comparison >= 201907L
namespace ops1 {
  struct Y {};
  int operator<=>(Y, int);
  bool operator==(Y, int);
}
namespace ops2 {
  struct Y {};
  bool operator==(Y, int);
  bool operator!=(Y, int);
}
#endif
namespace Incomplete {
  template <typename T> struct Holder { T t; };
  struct Z;
  void go(int, void*);
}
namespace C {
  struct G {};
  void qux(int, void*);
}
template <typename T> struct H : C::G {};
export module M;

export template <typename T>
void g(T t) {
  g_impl(t, Q::X{});  // ADL in definition finds Q::g_impl, g_impl not discarded
  // { dg-final { scan-lang-dump "Bindings '::Q::g_impl'" module } }

  t - Q::X{};  // Similarly for Q::operator-
  // { dg-final { scan-lang-dump "Bindings '::Q::operator-'" module } }
}

export template <typename T> struct Partial {
  template <typename U> static decltype(go_partial(T{}, U{})) f();
  template <typename U> static decltype(T{} / U{}) o();
};
// The instantantiation of Partial<Q::X> should emit go_partial and operator/
template struct Partial<Q::X>;
// { dg-final { scan-lang-dump "Bindings '::Q::go_partial'" module } }
// { dg-final { scan-lang-dump "Bindings '::Q::operator/'" module } }

#if __cpp_impl_three_way_comparison >= 201907L
export template <typename T>
void rewrite_ops(T t) {
  // Rewritten operators should also look for the ops they rewrite to.
  t < ops1::Y{};
  t != ops1::Y{};
  // { dg-final { scan-lang-dump {Bindings '::ops1::operator<=>'} module { target c++20 } } }
  // { dg-final { scan-lang-dump {Bindings '::ops1::operator=='} module { target c++20 } } }
}
export template <typename T>
void rewrite_ops_error(T t) {
  // Test we bind != to prevent considering == as a rewrite target.
  t == ops2::Y{};
  // { dg-final { scan-lang-dump "Bindings '::ops2::operator!='" module { target c++20 } } }
}
#endif

export template <typename T>
void incomplete(T t) {
  Incomplete::Holder<Incomplete::Z>* holder;
  go(t, holder);  // Shouldn't attempt to instantiate unnecessarily here
}

export template <typename T>
void needs_completion(T t) {
  H<int>* holder;
  // C::qux should be found via H<T>'s base, C::G.
  // But we don't see this because we don't attempt to complete the type.
  qux(t, holder);
  // { dg-final { scan-lang-dump "Bindings '::C::qux'" module { xfail *-*-* } } }
}
