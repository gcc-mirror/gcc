// C++20 NB comment US115
// { dg-do compile { target c++20 } }

template <class T> concept Any = true;

template <class T>
struct A
{
  friend void f() requires Any<T> { } // OK
  friend void g() requires Any<T>;    // { dg-error "" }
};

A<int> a;
