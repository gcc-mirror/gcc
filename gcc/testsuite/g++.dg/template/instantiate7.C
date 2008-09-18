// PR c++/19498
// Origin: Volker Reichelt  <reichelt@igpm.rwth-aachen.de>
// { dg-do compile }

template<typename T> struct A
{
    template<T&> struct B;  // { dg-error "reference to void" }
};

A<void> a;                  // { dg-message "instantiated" }
