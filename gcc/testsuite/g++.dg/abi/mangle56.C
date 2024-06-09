// { dg-do compile { target c++11 } }
// { dg-additional-options -fabi-compat-version=0 }

template <class T> T g(T t1, T t2) { return t2; }
// { dg-final { scan-assembler "_Z2f1IiEDTcl1gfp_ilEEET_" } }
template <class T> auto f1 (T t) -> decltype(g(t,{})) { return g(t,{}); }
// { dg-final { scan-assembler "_Z2f2IiEDTcl1gfp_tlT_EEES0_" } }
template <class T> auto f2 (T t) -> decltype(g(t,T{})) { return g(t,T{}); }

int main()
{
  f1(0);
  f2(0);
}
