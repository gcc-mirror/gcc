// Test that -Wparentheses does not give bogus warnings in the
// presence of templates.  Bug 17041.

// { dg-do compile }
// { dg-options "-Wparentheses" }

template<int> struct A
{
    int i;
    A() { if ((i = 0)) ; }
};

A<0> a;
