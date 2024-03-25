// { dg-do compile { target c++11 } }
// { dg-additional-options "-fabi-version=0 -fabi-compat-version=0" }

template<int...T> using N = int[sizeof...(T)];
template<int...A> void f(N<(A+1)...> &);

void g()
{
  int arr[3];

  // { dg-final { scan-assembler "_Z1fIJLi1ELi2ELi3EEEvRAsPXspplT_Li1EEE_i" } }
  f<1,2,3>(arr);
}
