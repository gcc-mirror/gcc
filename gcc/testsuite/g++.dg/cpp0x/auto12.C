// More auto/decltype mangling tests.
// { dg-options "-std=c++0x" }

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

int main()
{
  // { dg-final { scan-assembler  "_ZN1AIiE1fIiEEDTplsTT_sTiES2_" } }
  A<int>().f(1);
  // { dg-final { scan-assembler  "_ZN1AIiE2frIiEEDTplsTT_sTiES2_" } }
  A<int>().fr(1);
  // { dg-final { scan-assembler  "_ZN1AIiE3frrIiEEDTplsTT_sTiES2_" } }
  A<int>().frr(1);
  // { dg-final { scan-assembler  "_ZN1AIiE1gIiEEDTplsTT_sR.ES2_" } }
  A<int>().g(1);
  // { dg-final { scan-assembler  "_ZN1AIiE1hIiEEDTplsTT_sr1BIS2_E1iES2_" } }
  A<int>().h(1);
  // { dg-final { scan-assembler  "_ZN1AIiE1jIiEEDTplsTT_sRiES2_" } }
  A<int>().j(1);
}
