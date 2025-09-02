// PR c++/121351
// { dg-do compile { target c++20 } }

template<class T> concept C = true;

template<class T>
struct A;

template<class T>
struct A<T*> {
  template<class U> void f(U) requires C<T>; // #1
};

template<class T>
struct B : A<T> {
  using A<T>::f;
  template<class U> void f(U) requires C<int>; // #2
};

int main() {
  B<int*> b;
  b.f(42); // OK, #2 corresponds to and therefore hides #1
}
