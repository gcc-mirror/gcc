// Build don't link:

// Based on bug report by Miniussi <miniussi@ilog.fr>

class t {};

template <class T> struct A { typedef T t; typedef T u; };

template <class T> struct B : public A<T> {
  // according to [temp.dep.type], `t' and `u' cannot be dependent types,
  // and so there's no reason to delay lookup to specialization time.
  void f(t p); // this is ::t [temp.dep]/3
  void f(typename A<T>::t p); // gets bogus error - redefinition
  void g(u p); // ERROR - unknown type name
};
