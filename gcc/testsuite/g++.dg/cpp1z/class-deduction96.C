// PR c++/88252
// { dg-do compile { target c++17 } }

template<class T>
struct A {
  A(T&&);
  template<class U> A(T&&, U&&);
  template<class U> struct B;
};

template<class T>
A<T>::A(T&&) { }

template<class T>
template<class U>
A<T>::A(T&&, U&&) { }

template<class T>
template<class U>
struct A<T>::B {
  B(U&&);
  template<class V> B(U&&, V&&);
};

int i;

int main() {
  A{i}; // { dg-error "deduction|no match|rvalue reference" }
  A{i, 0}; // { dg-error "deduction|no match|rvalue reference" }
  A{0, i};
  A<int>::B{i}; // { dg-error "deduction|no match|rvalue reference" }
  A<int>::B{i, 0}; // { dg-error "deduction|no match|rvalue reference" }
  A<int>::B{0, i};
}
