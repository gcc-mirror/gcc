// PR c++/121351
// { dg-do compile { target c++20 } }

template<class T> concept C = true;

template<class T>
struct A {
  template<class U> void f(U) requires C<T>; // #1
};

template<class T>
struct B : A<T*> {
  using A<T*>::f;
  template<class U> void f(U) requires C<T>; // #2
};

int main() {
  B<int> b;
  b.f(42); // { dg-error "ambiguous" } #1 and #2 don't correspond
}
