// PR c++/39863
// { dg-do compile { target c++11 } }

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
