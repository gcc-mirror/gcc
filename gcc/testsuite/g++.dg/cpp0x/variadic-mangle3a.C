// Testcase from cxx-abi-dev.
// { dg-do compile { target c++11 } }
// { dg-options -fabi-version=9 }
// { dg-final { scan-assembler "_ZN1A1fIJiiEiJiiiEEEvRAstDpT1__iT0_S2_" } }

struct A {
  template<typename...T> using N = int[sizeof...(T)];
  template<typename...A, typename B, typename...C>
      void f(N<A..., B, C...> &, B, C...);
};
void g(A a) { int arr[6]; a.f<int, int>(arr, 1, 2, 3, 4); }
