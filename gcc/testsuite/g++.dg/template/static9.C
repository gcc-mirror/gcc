// PR c++/17524

template<typename T> struct A
{
  static const T i = 0; // { dg-error "declared void" "void" }
};

A<void> a; // { dg-message "required" }
