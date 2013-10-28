// More auto/decltype mangling tests.
// { dg-options "-std=c++11 -fabi-version=0" }

template <class T>
struct B
{
  static int i;
};

int&& x();

template <class T>
struct A
{
  static int i;
  static int &ir;
  static int &&irr;
  template <class U>
  auto f(U u) -> decltype (u + i);
  template <class U>
  auto fr(U u) -> decltype (u + ir);
  template <class U>
  auto frr(U u) -> decltype (u + irr);
  template <class U>
  auto g(U u) -> decltype (u + sizeof (i));
  template <class U>
  auto h(U u) -> decltype (u + B<U>::i);
  template <class U>
  auto j(U u) -> decltype (u + x());
};

template<class T> template<class U>
auto A<T>::f(U u) -> decltype (u + i)
{
  return u + i;
}

template <class... Args>
int f (Args... args);

template <class... Args>
auto g (Args... args) -> decltype (f ((args+1)...))
{
  return (f ((args+1)...));
}

int main()
{
  // { dg-final { scan-assembler  "_ZN1AIiE1fIiEEDTplfp_L_ZNS0_1iEEET_" } }
  A<int>().f(1);
  // { dg-final { scan-assembler  "_ZN1AIiE2frIiEEDTplfp_L_ZNS0_2irEEET_" } }
  A<int>().fr(1);
  // { dg-final { scan-assembler  "_ZN1AIiE3frrIiEEDTplfp_L_ZNS0_3irrEEET_" } }
  A<int>().frr(1);
  // { dg-final { scan-assembler  "_ZN1AIiE1gIiEEDTplfp_szL_ZNS0_1iEEET_" } }
  A<int>().g(1);
  // { dg-final { scan-assembler  "_ZN1AIiE1hIiEEDTplfp_sr1BIT_E1iES3_" } }
  A<int>().h(1);
  // { dg-final { scan-assembler  "_ZN1AIiE1jIiEEDTplfp_clL_Z1xvEEET_" } }
  A<int>().j(1);
  // { dg-final { scan-assembler  "_Z1gIJidEEDTcl1fspplfp_Li1EEEDpT_" } }
  g(42, 1.0);
}
