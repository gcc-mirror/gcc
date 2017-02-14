// Testcase from cxx-abi-dev.
// { dg-do compile { target c++11 } }

struct A {
  template<int...T> using N = int[sizeof...(T)];
  template<int...A> void f(N<A...> &);

  template<typename...T> using M = int[sizeof...(T)];
  template<typename...A> void g(M<A...> &);
};
void g(A a)
{
  int arr[3];
  // { dg-final { scan-assembler "_ZN1A1fIJLi1ELi2ELi3EEEEvRAsZT__i" } }
  a.f<1,2,3>(arr);
  // { dg-final { scan-assembler "_ZN1A1gIJiiiEEEvRAsZT__i" } }
  a.g<int,int,int>(arr);
}
