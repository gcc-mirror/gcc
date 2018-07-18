// { dg-do link { target c++11 } }

struct X { X(X &&); };
struct A {
  A() {}
  A(const A&);       // #1
  A(A &&) = default; // #2, defined as deleted (12.8 [class.copy])
  template<typename T> A(T &&);	// #3
  union { X x; };
};
struct B : A {
  using A::A;
  B(...) {}
};

int main() {
  B b = A(); // calls B::B(...): #1, #2, and #3 are excluded from candidate set
}
