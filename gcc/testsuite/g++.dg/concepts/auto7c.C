// { dg-do compile { target c++17 } }
// { dg-additional-options -fconcepts }

template <class T>
concept True = true;

template <class T> struct A { };
void f(A<True auto> a) { } // { dg-error "use of .auto. in template argument" }
int main()
{
  f(A<int>());
}
