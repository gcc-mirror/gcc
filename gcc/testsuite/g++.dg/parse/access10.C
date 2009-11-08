// PR c++/18698
// The compiler was giving an error message for invalid syntax
// that irrelevantly talked about using-declarations.

template<int> struct A
{
    ::A~();			// { dg-bogus "using-declaration" }
};

// Instead of the bogus error we get a different error.
// { dg-error "template-name" "" { target *-*-* } 7 }
