// PR c++/121351
// { dg-do compile { target c++20 } }

template<class T> concept C = true;

template<class T>
struct A {
  template<class U> void f(U) requires C<T> = delete; // #1
};

struct B : A<int> {
  using A::f;
  template<class U> void f(U) requires C<int>; // #2
};

int main() {
  B b;
  b.f(42); // OK, #2 corresponds to and therefore hides #1
}
