// { dg-do compile { target c++14 } }

template <class T> struct A { };
void f(A<auto> a) { }		// { dg-error "auto. in template argument" }
int main()
{
  f(A<int>());
}
