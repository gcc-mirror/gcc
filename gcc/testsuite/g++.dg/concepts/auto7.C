// { dg-do compile { target c++14 } }
// { dg-additional-options -fconcepts }

template <class T> struct A { };
void f(A<auto> a) { } // { dg-error "use of .auto. in template argument" }
int main()
{
  f(A<int>());
}
