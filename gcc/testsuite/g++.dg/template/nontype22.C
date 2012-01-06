// PR c++/44629
// The proper mangling is unclear.

template<typename T> int cmp1(T a, T b);
template<typename T, int (*cmp)(T, T) = cmp1> struct A { };
template <typename T> void f (A<T> &);
void g()
{
  A<char> a;
  f(a);
}
