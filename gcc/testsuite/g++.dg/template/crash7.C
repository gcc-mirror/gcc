// { dg-do compile }

// Origin: Volker Reichelt <reichelt@igpm.rwth-aachen.de>

// PR c++/10108: ICE in tsubst_decl for error due to non-existence
// nested type.

template <typename> struct A
{					// { dg-error "candidates" }
    template <typename> A(typename A::X) {} // { dg-error "no type" }
};

A<void> a;	// { dg-error "instantiated|no match" }
