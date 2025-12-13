// PR c++/123075
// { dg-do compile { target c++20 } }
// { dg-additional-options "-fkeep-inline-functions" }

template <typename T>
concept r = []{ return true; }();

template <typename T, typename U>
inline auto foo() {
  static_assert(r<T>);
  r<U>;
  return []{ return false; };
}

template <typename T>
struct S {
  static_assert(r<T>);
  decltype([]{ return true; }) l;
};
S<char> s;

bool use = (foo<int, double>()() || s.l());

// There should only be one lambda keyed to 'foo()' and 'S::l'
// { dg-final { scan-assembler {_ZZ3fooIidEDavENKUlvE_clEv:} } }
// { dg-final { scan-assembler {_ZNK1SIcEUlvE_clEv:} } }
// { dg-final { scan-assembler-not {_ZZ3fooIidEDavENKUlvE0_clEv:} } }
// { dg-final { scan-assembler-not {_ZNK1SIcEUlvE0_clEv:} } }
