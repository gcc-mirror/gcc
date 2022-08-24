// P0892R2
// { dg-do compile }
// { dg-options "-std=c++2a -pedantic" }

template<typename T>
struct A {
  explicit A(const T&, ...) noexcept;
  A(T&&, ...);
};

int i;
A a1 = { i, i }; // { dg-error "deduction|cannot|no match" }
A a2{ i, i };
A a3{ 0, i };
A a4 = { 0, i };

template<typename T> A(const T&, const T&) -> A<T&>;
template<typename T> explicit A(T&&, T&&) -> A<T>;

A a5 = { 0, 1 }; // { dg-error "deduction|ambiguous" }
A a6{ 0, 1 };

template<typename T>
struct B {
  template<typename U> using TA = T;
  template<typename U> B(U, TA<U>);
};

B b{(int *)0, (char *)0};	// { dg-error "deduction|no match" }
