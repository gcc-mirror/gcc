// { dg-do compile { target c++11 } }
// { dg-options "-fabi-version=0" }

template<typename T, int (*cmp)(T, T)> struct A { };
struct B {
  template<typename T> static int cmp1(T a, T b);
  static int cmp2(char a, char b);
  // { dg-final { scan-assembler "_ZN1B1fIcEEvR1AIT_X4cmp1EE" } }
  template <typename T> static void f (A<T,cmp1> &);
  // { dg-final { scan-assembler "_ZN1B1gIcEEvR1AIT_XsrS_4cmp1EE" } }
  template <typename T> static void g (A<T,B::cmp1> &);
  // { dg-final { scan-assembler "_ZN1B1fIcEEvR1AIT_L_ZNS_4cmp2EccEE" } }
  template <typename T> static void f (A<T,cmp2> &);
  // { dg-final { scan-assembler "_ZN1B1gIcEEvR1AIT_L_ZNS_4cmp2EccEE" } }
  template <typename T> static void g (A<T,B::cmp2> &);
};

void g()
{
  A<char,B::cmp1> a;
  B::f(a);
  B::g(a);
  A<char,B::cmp2> a2;
  B::f(a2);
  B::g(a2);
}
