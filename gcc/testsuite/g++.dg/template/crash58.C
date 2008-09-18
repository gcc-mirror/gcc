//PR 26938

template<int, int = 0> struct A;  // { dg-message "previous declaration" }

template<int> struct A            // { dg-error "template" }
{
  A();
};

A<0> a;                           // { dg-error "incomplete type" }
