// { dg-do compile }

// Origin: Volker Reichelt <reichelt@igpm.rwth-aachen.de>

// PR c++/10108: ICE in tsubst_decl for error due to non-existence
// nested type.

template <typename> struct A
{
    template <typename> A(typename A::X) {} // { dg-error "no type" }
};

// We currently don't give the "no match" error because we don't add the
// invalid constructor template to TYPE_METHODS.
A<void> a;			// { dg-message "required" }
