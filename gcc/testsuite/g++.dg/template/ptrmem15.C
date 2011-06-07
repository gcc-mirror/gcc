// PR c++/19894
// Origin: Volker Reichelt <reichelt@igpm.rwth-aachen.de>

template<typename T> struct A
{
    T A::* p;  // { dg-error "void" }
};

A<void> a;     // { dg-message "required" }
