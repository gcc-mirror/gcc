// { dg-do assemble  }

extern "C" int printf(const char *, ...);
template <class T> struct A {
  typedef typename T::X B; // { dg-error "" } not a class
  A(double);
};
 
template <class T> void xxx(typename A<T>::B);
 
template <class T> struct B {
  friend void xxx<T>(T); // { dg-error "" } does not match any template
};
 
template struct B<double>;
