// { dg-do compile }

// Origin: Volker Reichelt <reichelt@igpm.rwth-aachen.de>

// PR c++/10554: ICE for member using declaration with failed
// scope name lookup.

template <typename> struct A
{
    typedef A X;
    void foo();
};

template <typename T> struct B : A<T>
{
    using X::foo; // { dg-error "declared|nested-name-specifier|non-member" }
};
