// PR c++/120917
// { dg-do compile { target c++14 } }
// { dg-additional-options "-fconcepts -Wno-abbreviated-auto-in-template-arg" }

template <class T> struct A { };
void f(A<auto> a) { }
int main()
{
  f(A<int>());
}
