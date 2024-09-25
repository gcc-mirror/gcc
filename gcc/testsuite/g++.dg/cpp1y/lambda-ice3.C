// PR c++/109790
// This used to work until GCC 10; force the usage of ABI 15 (the highest
// usable in GCC 10) and check that the mangling (actually wrong; see
// lambda-ice3a.C) matches that of GCC 10's default ABI version (14).

// { dg-do compile { target c++14 } }
// { dg-additional-options "-fabi-version=15" }

auto ll = [](auto ... ){};
template <class _Impl, class _Args>
  void mm(void (_Impl::*__p)(_Args) const);
template <class _Ts>
using __impl_for = decltype(mm(&decltype(ll)::operator()<_Ts>));
template <class _Ts> __impl_for<_Ts> f() { }
void aaa() {
  f<int>();
}

// { dg-final { scan-assembler "_Z1fIiEDTcl2mmadsrN2llMUlDpT_E_EclIT_EEEv" } }
