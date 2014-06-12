// { dg-do compile { target c++11 } }
// { dg-options "-fabi-version=0 -Wabi=2" }

template<typename T> int cmp1(T a, T b);
int cmp2(char a, char b);
template<typename T, int (*cmp)(T, T)> struct A { };
// { dg-final { scan-assembler "\n_?_Z1fIcEvR1AIT_X4cmp1EE\[: \t\n\]" } }
template <typename T> void f (A<T,cmp1> &) {}
// { dg-final { scan-assembler "\n_?_Z1fIcEvR1AIT_L_Z4cmp2ccEE\[: \t\n\]" } }
template <typename T> void f (A<T,cmp2> &) {} // { dg-warning "mangle" }
void g()
{
  A<char,cmp1> a;
  f(a);
  A<char,cmp2> a2;
  f(a2);
}
