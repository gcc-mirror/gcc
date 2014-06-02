// DR 1227, PR c++/57543
// { dg-do compile { target c++11 } } 

struct S
{
  template <class T> struct A { using X = typename T::X; };  // { dg-error "not a class" }
  template <class T> typename T::X f(typename A<T>::X);
  template <class T> void f(...) { }
  template <class T> auto g(typename A<T>::X) -> typename T::X;  // { dg-message "required" }
  template <class T> void g(...) { }

  void h()
  {
    f<int>(0);  // OK
    g<int>(0);  // { dg-message "required" }
  }
};
