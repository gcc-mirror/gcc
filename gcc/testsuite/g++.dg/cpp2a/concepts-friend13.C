// Verify we substitute the correct outer template arguments
// when instantiating a constrained template friend declared
// inside a partial specialization.
// { dg-do compile { target c++20 } }

template<class U>
  requires __is_same(int*, U)
void f() { };

template<class T>
struct A;

template<class T>
struct A<T*> {
  template<class U>
    requires __is_same(T, U)
  friend void f() { } // { dg-bogus "redefinition" }
};

template struct A<int*>;
