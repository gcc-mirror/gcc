// PR c++/57573

struct A { };
struct B { A a; };

void f(A*) { }

template<class T>
void g()
{
  B b;
  f(&(b.a));
}
