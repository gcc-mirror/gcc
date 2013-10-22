// { dg-options "-std=gnu++11" }
template<class... Types> struct B { // { dg-error "declaration of" }
  void f3();
  void f4();
};

template<class... Types> void B<Types...>::f3() { } // OK
template<class... Types> void B<Types>::f4() { } // { dg-error "invalid" }
