// Tests for late-specified return type.
// { dg-options "-std=c++0x -fabi-version=6" }

auto f() -> int
{
  return 0;
}

template<class T, class U>
auto add(T t, U u) -> decltype (t+u)
{
  return t+u;
}

template<class T, class U>
decltype(T()+U()) add2(T t, U u)
{
  return t+u;
}

template <class T, class U>
U ag (T, U)
{
  return U();
}

template<class T, class U>
auto add3(T t, U u) -> decltype (ag(t,u))
{
  return ag(t,u);
}

template<class T, class U>
decltype(*(T*)0+*(U*)0) add4(T t, U u)
{
  return t+u;
}

template <class T>
struct A
{
  T f() {}
  template <class U>
  T g() {}
  template <class V>
  struct B
  {
    int MEM;
  };
};

template <class T>
auto f(T* t) -> decltype (t->f())
{
  return t->f();
}

template <class T>
auto g(T t) -> decltype (t.f())
{
  return t.f();
}

template <class T, class U>
auto h(T t, U u) -> decltype (t.template g<U>())
{
  return t.template g<U>();
}

struct D { };
struct C: public A<int>::B<D>
{
};

template <class T, class U, class V>
auto k(T t, U u, V v) -> decltype (t.U::template B<V>::MEM)
{
  return t.U::template B<V>::MEM;
}

template <class T>
auto l(T t) -> decltype (t)
{
  return t;
}

template <class T, T u>
auto m(T t) -> decltype (u)
{
  return t;
}

A<int> a, *p;

int main()
{
  // { dg-final { scan-assembler  "_Z3addIidEDTplfp_fp0_ET_T0_" } }
  auto i = add(1, 2.0);
  // { dg-final { scan-assembler "_Z4add4IidEDTpldecvPT_Li0EdecvPT0_Li0EES0_S2_" } }
  auto i4 = add4(1, 2.0);
  // { dg-final { scan-assembler "_Z4add2IidEDTplcvT__EcvT0__EES0_S1_" } }
  auto i2 = add2(1, 2.0);
  // { dg-final { scan-assembler "_Z4add3IidEDTcl2agfp_fp0_EET_T0_" } }
  auto i3 = add3(1, 2.0);
  // { dg-final { scan-assembler "_Z1fI1AIiEEDTclptfp_1fEEPT_" } }
  f(p);
  // { dg-final { scan-assembler "_Z1gI1AIiEEDTcldtfp_1fEET_" } }
  g(a);
  // { dg-final { scan-assembler "_Z1hI1AIiEdEDTcldtfp_1gIT0_EEET_S2_" } }
  h(a,1.0);
  // { dg-final { scan-assembler "_Z1kI1C1AIiE1DEDtdtfp_srNT0_1BIT1_EE3MEMET_S4_S6_" } }
  k( C(), A<int>(), D() );
  // { dg-final { scan-assembler "_Z1lIiEDtfp_ET_" } }
  l(1);
  // { dg-final { scan-assembler "_Z1mIiLi1EEDtT0_ET_" } }
  m<int,1>(1);
}
