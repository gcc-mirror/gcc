// { dg-do compile }

// Origin: Volker Reichelt <reichelt@igpm.rwth-aachen.de>

// PR c++/10553: ICE processing typename with context error.

template <typename> struct A {};

template <typename> struct B
{
    typedef A<typename X::Y> C; // { dg-error "declared|invalid|no type|expected" }
};
