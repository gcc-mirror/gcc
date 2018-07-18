// PR c++/23191
// Origin: Volker Reichelt  <reichelt@igpm.rwth-aachen.de>
// { dg-do compile { target c++14_down } }

template<typename T> struct A
{
    void foo() throw(typename T::X);  // { dg-error "not a class" }
};				      // { dg-warning "deprecated" "" { target c++11 } .-1 }

A<void> a;                            // { dg-message "required" }
