// Tests for late-specified return type.
// { dg-options "-std=c++0x" }

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

A<int> a, *p;

int main()
{
  // { dg-final { scan-assembler "_Z3addIidEDTplsTT_sTT0_ES0_S1_" } }
  auto i = add(1, 2.0);
  // { dg-final { scan-assembler "_Z4add2IidEDTplcvT_vcvT0_vES0_S1_" } }
  auto i2 = add2(1, 2.0);
  // { dg-final { scan-assembler "_Z4add3IidEDTclL_Z2agEsTT_sTT0_EES0_S1_" } }
  auto i3 = add3(1, 2.0);
  // { dg-final { scan-assembler "_Z1fI1AIiEEDTclptsTPT_1fEES3_" } }
  f(p);
  // { dg-final { scan-assembler "_Z1gI1AIiEEDTcldtsTT_1fEES2_" } }
  g(a);
  // { dg-final { scan-assembler "_Z1hI1AIiEdEDTcldtsTT_1gIT0_EEES2_S3_" } }
  h(a,1.0);
  // { dg-final { scan-assembler "_Z1kI1C1AIiE1DEDtdtsTT_srNT0_1BIT1_EE3MEMES4_S5_S7_" } }
  k( C(), A<int>(), D() );
}
