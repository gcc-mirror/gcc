// Testcase from cxx-abi-dev.
// { dg-do compile { target c++11 } }

struct A {
  template<int...T> using N = int[sizeof...(T)];
  template<int...A> void f(N<A...> &);

  template<typename...T> using M = int[sizeof...(T)];
  template<typename...A> void g(M<A...> &);
};

template<typename ...T> using N = int[sizeof...(T)];
template<typename ...T> void f(N<T...> &);
// equivalent to template<typename ...T> void f(int(&)[sizeof...(T)])

void g(A a)
{
  int arr[3];
  // { dg-final { scan-assembler "_ZN1A1fIJLi1ELi2ELi3EEEEvRAsZT__i" } }
  a.f<1,2,3>(arr);
  // { dg-final { scan-assembler "_ZN1A1gIJiiiEEEvRAsZT__i" } }
  a.g<int,int,int>(arr);
  // { dg-final { scan-assembler "_Z1fIJiiiEEvRAsZT__i" } }
  f<int,int,int>(arr);
}

