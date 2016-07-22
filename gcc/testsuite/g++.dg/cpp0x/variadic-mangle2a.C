// Testcase from cxx-abi-dev.
// { dg-do compile { target c++11 } }
// { dg-options "-fabi-version=9" }

struct A {
  template<int...T> using N = int[sizeof...(T)];
  template<int...A> void f(N<A...> &);

  template<typename...T> using M = int[sizeof...(T)];
  template<typename...A> void g(M<A...> &);
};
void g(A a)
{
  int arr[3];
  // { dg-final { scan-assembler "_ZN1A1fIJLi1ELi2ELi3EEEEvRAszspT__i" } }
  a.f<1,2,3>(arr);
  // { dg-final { scan-assembler "_ZN1A1gIJiiiEEEvRAstDpT__i" } }
  a.g<int,int,int>(arr);
}
