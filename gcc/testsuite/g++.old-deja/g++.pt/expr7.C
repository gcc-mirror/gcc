// Build don't link:

template<int I> struct A { };
template<int I, int J> int f(A<I+J>);
int g() {
  A<3> a;
  return f<1,2>(a);
}
