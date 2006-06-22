//PR c++/27821

struct A
{
    template<void (A::*)()> struct B {};
    void ::foo(); // { dg-error "invalid use" }
    B<&A::foo> b; // { dg-errror "incomplete type|template argument" }
};
 
