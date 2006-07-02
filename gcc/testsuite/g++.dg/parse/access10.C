// PR c++/18698
// The compiler was giving an error message for invalid syntax
// that irrelevantly talked about using-declarations.

template<int> struct A
{
    ::A~();			// { dg-bogus "using-declaration" }
};

// Instead of the bogus error we get 3 separate errors.
// { dg-error "no type" "" { target *-*-* } 7 }
// { dg-error "::" "" { target *-*-* } 7 }
// { dg-error "~" "" { target *-*-* } 7 }
