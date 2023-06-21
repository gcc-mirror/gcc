// PR c++/109420

struct A {
  struct X { };
  int X;
};

struct B {
  enum E { };
  enum F { E };
};

template<class T, class U>
void f() {
  struct T::X x; // OK, lookup ignores the data member 'int A::X'
  enum U::E e;   // OK, lookup ignores the enumerator 'B::F::E'
}

template void f<A, B>();
