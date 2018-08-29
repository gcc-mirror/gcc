// { dg-do compile { target c++11 } }
template<class... Types> struct B {
  void f3();
  void f4();
};

template<class... Types> void B<Types...>::f3() { } // OK
template<class... Types> void B<Types>::f4() { } // { dg-error "packs not expanded" }
