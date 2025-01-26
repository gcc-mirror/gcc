// { dg-do compile { target c++20 } }
// { dg-additional-options "-fkeep-inline-functions" }
// See also https://github.com/itanium-cxx-abi/cxx-abi/pull/85

struct A {
  decltype([]{ return 1; }) f;
};

struct B : decltype([]{ return 2; }) {
  decltype([]{ return 3; }) f;
};

template <typename T>
struct C : decltype([]{ return 4; }) {
  decltype([]{ return 5; }) f;
};

template struct C<int>;
template struct C<double>;

// { dg-final { scan-assembler {_ZNK1AUlvE_clEv:} } }
// { dg-final { scan-assembler {_ZNK1BUlvE_clEv:} } }
// { dg-final { scan-assembler {_ZNK1BUlvE0_clEv:} } }
// { dg-final { scan-assembler {_ZNK1CIiEUlvE_clEv:} } }
// { dg-final { scan-assembler {_ZNK1CIiEUlvE0_clEv:} } }
// { dg-final { scan-assembler {_ZNK1CIdEUlvE_clEv:} } }
// { dg-final { scan-assembler {_ZNK1CIdEUlvE0_clEv:} } }
