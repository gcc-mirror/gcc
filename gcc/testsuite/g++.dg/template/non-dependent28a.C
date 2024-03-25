// PR c++/111929

struct A { operator int(); };

template<class>
void f() {
  new int[A()];
}
