// PR c++/34776

template<typename T> struct A
{
  T::X<0> x; // { dg-error "non-template" }
  // { dg-message "T::template" "" { target *-*-* } .-1 }
  // { dg-prune-output "is not a class" }
};

A<int*> a;
