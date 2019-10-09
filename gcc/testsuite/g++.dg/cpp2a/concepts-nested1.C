// { dg-do compile { target concepts } }

namespace N { template <class T> concept True = true; }
template <class T> struct A { };

template <class T>
requires N::True<T> && requires { typename A<T>; }
void f();

int main()
{
  f<int>();
}
