// PR c++/39863
// { dg-options -std=c++0x }

template <typename... T>
struct A {};

template <typename T, typename U>
struct S {};

template <typename... T, typename... U>
A< S<T, U>... > f(U... u)
{ return A< S<T, U>... >(); }

int main()
{
  f<int>(0.0);
}
