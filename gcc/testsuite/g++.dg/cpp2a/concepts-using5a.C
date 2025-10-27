// PR c++/121351
// { dg-do compile { target c++20 } }
// A version of concepts-using5a.C where B instead of A is a template.

template<class T> concept C = true;

struct A {
  template<class U> void f(U) requires C<int> = delete; // #1
};

template<class T>
struct B : A {
  using A::f;
  template<class U> void f(U) requires C<T>; // #2
};

int main() {
  B<int> b;
  b.f(42); // OK, #2 corresponds to and therefore hides #1
}
