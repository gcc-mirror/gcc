// { dg-do compile }
// PR c++/7874: Don't inject friend functions into global name space.

namespace N { template<typename T> struct A { friend void f(A) { }; }; }
int main()
{
   N::A<int> a;
   N::f(a);		// { dg-error "not a member" }
}

struct S { friend void g(); friend void h(S); };
struct T { friend void g(); friend void h(T); };
void i() {
  g();			// { dg-error "not declared" }
  S s;
  h(s);
  T t;
  h(t);
}
