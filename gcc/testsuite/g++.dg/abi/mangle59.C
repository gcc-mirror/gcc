// { dg-do compile { target c++11 } }
// { dg-options "-fabi-version=0" }

// { dg-final { scan-assembler "_Z1fIiEDTcmdlfp_psfp_EPT_" } }
template <class T> auto f (T* p) -> decltype(delete p, +p) { return p; }
// { dg-final { scan-assembler "_Z1gIiEDTcmgsdlfp_psfp_EPT_" } }
template <class T> auto g (T* p) -> decltype(::delete p, +p) { return p; }
// { dg-final { scan-assembler "_Z1hIiEDTcmdafp_psfp_EPT_" } }
template <class T> auto h (T* p) -> decltype(delete[] p, +p) { return p; }
// { dg-final { scan-assembler "_Z1iIiEDTcmgsdafp_psfp_EPT_" } }
template <class T> auto i (T* p) -> decltype(::delete[] p, +p) { return p; }

int main()
{
  int x;
  f(&x);
  g(&x);
  h(&x);
  i(&x);
}
