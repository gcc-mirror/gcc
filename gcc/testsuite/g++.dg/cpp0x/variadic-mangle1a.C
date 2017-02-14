// Test for sZ mangling.
// { dg-do compile { target c++11 } }
// { dg-final { scan-assembler "_Z1fIJidEEv1AIXstDpT_EE" } }
// { dg-options -fabi-version=9 }

template <int I> struct A { };
template <typename... Ts> void f(A<sizeof...(Ts)>);

int main()
{
  f<int,double>(A<2>());
}
