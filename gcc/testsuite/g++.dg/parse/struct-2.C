// Origin: Volker Reichelt <reichelt@igpm.rwth-aachen.de>
// PR c++/18731

template<typename T> struct A
{
    struct T::B {}; // { dg-error "invalid class name" }
};
