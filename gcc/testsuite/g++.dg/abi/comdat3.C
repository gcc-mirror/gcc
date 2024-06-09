// PR lto/113208
// { dg-do compile { target { c++11 && { *-*-*gnu* } } } }
// { dg-additional-options "-O2" }
// { dg-final { scan-assembler "_ZN1M1SINS_1P1TELN1N1LE2EEC5Ev,comdat" } }
// { dg-final { scan-assembler-not "_ZN1M1SINS_1P1TELN1N1LE2EEC1Ev,comdat" } }
// { dg-final { scan-assembler-not "_ZN1M1SINS_1P1TELN1N1LE2EEC2Ev,comdat" } }

namespace N { enum L { L1, L2, L3 } const O = L3; }
namespace M {
  using N::O;
  using N::L;
  template <typename, L = O>
  struct S { constexpr S () {} };
  namespace P {
    struct T;
    struct U { S<T> u; };
    void foo () { U (); }
  }
  extern template class S<P::T>;
}
namespace p = M::P;
template class M::S<p::T>;
