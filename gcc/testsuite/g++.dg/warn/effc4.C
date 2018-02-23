// PR c++/84364
// { dg-do compile }
// { dg-options "-Weffc++" }

template <typename T>
struct A {
  A &operator=(A<T>& f) {
    return *this;	// { dg-bogus "should return a reference to" }
  }
};
