// PR c++/21008, DR 515

struct A {
  int foo_;
};
template <typename T> struct B: public A { };
template <typename T> struct C: B<T> {
  int foo() {
    return A::foo_;  // #1
  }
};
int f(C<int>* p) {
  return p->foo();
}
