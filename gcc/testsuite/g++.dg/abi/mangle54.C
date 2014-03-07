// { dg-do compile { target c++11 } }
// { dg-options "-fabi-version=0" }

int i;
// { dg-final { scan-assembler "_Z2f1IiEDTppfp_ET_" } }
template <class T> auto f1 (T t) -> decltype(t++) { return i; }
// { dg-final { scan-assembler "_Z2f2IiEDTpp_fp_ET_" } }
template <class T> auto f2 (T t) -> decltype(++t) { return i; }
// { dg-final { scan-assembler "_Z2f3IiEDTmmfp_ET_" } }
template <class T> auto f3 (T t) -> decltype(t--) { return i; }
// { dg-final { scan-assembler "_Z2f4IiEDTmm_fp_ET_" } }
template <class T> auto f4 (T t) -> decltype(--t) { return i; }

int main()
{
  f1(0);
  f2(0);
  f3(0);
  f4(0);
}
