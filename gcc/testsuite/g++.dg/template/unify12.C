// PR c++/116710
// { dg-do compile { target c++11 } }

template<class T> struct A : T {};

template<class T>
void f(void (*)(T &), typename A<T>::type * = 0);

void f(...);

void g(int &&);

void q() { f(g); } // OK

template<class T>
struct B { operator B<T&>(); };

template<class T>
void h(B<T&>);

int main() {
  B<int&&> b;
  h(b); // { dg-error "no match" }
}
