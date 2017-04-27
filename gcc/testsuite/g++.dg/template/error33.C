// PR c++/24791

template<int> struct A
{
    static int i;
    A() { ++i; }
};

template<int> int A<0>::i(0);	// { dg-error "template" "error" }
// { dg-message "note" "note" { target *-*-* } .-1 }

A<0> a;
