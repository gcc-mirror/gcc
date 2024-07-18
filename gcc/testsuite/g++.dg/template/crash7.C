// { dg-do compile }

// Origin: Volker Reichelt <reichelt@igpm.rwth-aachen.de>

// PR c++/10108: ICE in tsubst_decl for error due to non-existence
// nested type.

template <typename> struct A
{
    template <typename> A(typename A::X) {} // { dg-error "does not name a type" }
};

A<void> a;			// { dg-error "no match" }
