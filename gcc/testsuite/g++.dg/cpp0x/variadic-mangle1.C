// Test for sZ mangling.
// { dg-do compile { target c++11 } }
// { dg-final { scan-assembler "_Z1fIJidEEv1AIXsZT_EE" } }

template <int I> struct A { };
template <typename... Ts> void f(A<sizeof...(Ts)>);

int main()
{
  f<int,double>(A<2>());
}
