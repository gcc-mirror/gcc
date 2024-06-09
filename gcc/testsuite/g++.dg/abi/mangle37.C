// Testcase for mangling of expressions involving operator names.
// { dg-do compile { target c++11 } }
// { dg-options "-fabi-version=10 -fabi-compat-version=10" }
// { dg-final { scan-assembler "_Z1fI1AEDTclonplfp_fp_EET_" } }
// { dg-final { scan-assembler "_Z1gI1AEDTclonplIT_Efp_fp_EES1_" } }
// { dg-final { scan-assembler "_Z1hI1AEDTcldtfp_miEET_" } }
// { dg-final { scan-assembler "_Z1iI1AEDTcldtfp_srT_miEES1_" } }
// { dg-final { scan-assembler "_Z1jI1AEDTcldtfp_cvPT_EES1_" } }

struct A {
  void operator-();
  template <class T>
  operator T();
};
template <class T>
T operator+(T,T);

template <class T>
auto f (T t) -> decltype(operator+(t,t));
template <class T>
auto g (T t) -> decltype(operator+<T>(t,t));
template <class T>
auto h (T t) -> decltype(t.operator-());
template <class T>
auto i (T t) -> decltype(t.T::operator-());
template <class T>
auto j (T t) -> decltype(t.operator T*());

int main()
{
  f(A());
  g(A());
  h(A());
  i(A());
  j(A());
}
