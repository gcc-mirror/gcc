// { dg-do compile { target c++20 } }
// { dg-additional-options -fabi-compat-version=0 }

template <class T> concept C = true;
template <class T, class U> concept C2 = true;
template <class T> concept D = true;
template <class T> concept E = true;
template <class T> concept F = true;
template <class T> using Ref = T&;

// { dg-final { scan-assembler "_Z1fIiQ1CIT_EEvv" } }
template <class T> requires C<T> void f() {}
template void f<int>();

// { dg-final { scan-assembler "_Z2f2ITk1CiEvv" } }
template <C T> void f2() {}
template void f2<int>();

// { dg-final { scan-assembler "_Z2f3IiEvvQ1CIT_E" } }
template <class T> void f3() requires C<T> {}
template void f3<int>();

// { dg-final { scan-assembler "_Z2f4ITk1CiEvT_" } }
void f4(C auto c) {}
template void f4(int);

// ??? The constraints end up out of order in the mangled name, may
// need to change the equivalence rule.
// { dg-final { scan-assembler "_Z2f5ITk1CicTk1EfTk1FsQ1DIT0_EEvT1_T2_" } }
template <C T, class U> requires D<U> void f5(E auto c, F auto f) {}
template void f5<int,char>(float,short);

// { dg-final { scan-assembler "_Z2f6ITk2C2IiEsEvv" } }
template <C2<int> T> void f6() {}
template void f6<short>();

// { dg-final { scan-assembler "_ZN1AIiE1fEvQ1CIT_E" } }
template <class T> struct A {
  void f() requires C<T> { };
};
template struct A<int>;

// { dg-final { scan-assembler "_Z1gIiQrqXcvT__ETRS0_Q1CIS0_EXpscvS0__ENR1CEEvv" } }
template <class T>
requires requires { T();
		    typename Ref<T>;
		    requires C<T>;
		    { +T() } noexcept -> C;
}
void g() {}
template void g<int>();

// { dg-final { scan-assembler "_Z1hIiQrQT__Xpsfp_EEvv" } }
template <class T>
requires requires (T t) { +t; }
void h() {}
template void h<int>();

// { dg-final { scan-assembler "_Z3fn1IiEvT_QrQS0__XpsfL0p_Xpsfp_E" } }
template <class T>
void fn1(T t1)
  requires requires (T t2) { +t1; +t2; }
{}
template void fn1<int>(int);

// { dg-final { scan-assembler "_Z3fn3IiTk2C2IDtfL0p_EEiEvT_T0_" } }
template<typename T> void fn3(T t, C2<decltype(t)> auto) {}
template void fn3(int, int);

// { dg-final { scan-assembler "_Z3fn4IiiEvT_T0_Q2C2IS1_FDTcl3fn3fL0p_fp_EES0_EE" } }
template<typename T, typename U> void fn4(T t, U u)
  requires C2<U, auto (T u) -> decltype(fn3(t, u))> {}
template void fn4(int, int);

// { dg-final { scan-assembler "_Z3fn5ITpTk1CJicfEEvDpT_" } }
template<C... T> void fn5(T...) { }
template void fn5(int,char,float);

// { dg-final { scan-assembler "_ZN2A2IiE1BIiE1fIiiEEvvQ2C2IT_TL1_0_E" } }
template <class T> struct A2 {
  template <class X> struct B {
    template <class U, class V> void f() requires C2<T,V> {}
  };
};
template void A2<int>::B<int>::f<int,int>();

template<C auto N> void f7() {}
// { dg-final { scan-assembler "_Z2f7ITnDk1CLi5EEvv" } }
template void f7<5>();
