// PR c++/23191
// Origin: Volker Reichelt  <reichelt@igpm.rwth-aachen.de>
// { dg-do compile }

template<typename T> struct A
{
    void foo() throw(typename T::X);  // { dg-error "not a class" }
};

A<void> a;                            // { dg-message "instantiated" }
