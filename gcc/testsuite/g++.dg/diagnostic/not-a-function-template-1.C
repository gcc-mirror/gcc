// { dg-do compile { target c++14 } }

template<typename=int> int A;  // { dg-message "28:variable template" }

template int A<>();  // { dg-error "14:template<class>" }

struct B {
  friend int A<>();  // { dg-error "14:specialization" }
};
