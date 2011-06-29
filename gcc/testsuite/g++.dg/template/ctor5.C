// PR c++/24278

template<typename T> struct A
{
  A() : T(0) {} // { dg-error "base" }
};

A<int*> a; // { dg-message "required" }
