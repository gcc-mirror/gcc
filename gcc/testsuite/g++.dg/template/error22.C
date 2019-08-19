//PR c++/27821

struct A
{
    template<void (A::*)()> struct B {};
    void ::foo(); // { dg-error "10:invalid use" }
    B<&A::foo> b; // { dg-error "incomplete type|template argument" }
};
 
