// PR c++/109790
// Up to GCC 10, the mangling would be missing the "on" marker, hence be wrong.
// Check that this is fixed with the latest ABI.

// { dg-do compile { target c++14 } }

auto ll = [](auto ... ){};
template <class _Impl, class _Args>
  void mm(void (_Impl::*__p)(_Args) const);
template <class _Ts>
using __impl_for = decltype(mm(&decltype(ll)::operator()<_Ts>));
template <class _Ts> __impl_for<_Ts> f() { }
void aaa() {
  f<int>();
}

// { dg-final { scan-assembler "_Z1fIiEDTcl2mmadsrN2llMUlDpT_E_EonclIT_EEEv" } } 
