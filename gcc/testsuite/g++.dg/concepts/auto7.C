// { dg-do compile { target c++14 } }
// { dg-additional-options -fconcepts-ts }

template <class T> struct A { };
void f(A<auto> a) { }
int main()
{
  f(A<int>());
}
