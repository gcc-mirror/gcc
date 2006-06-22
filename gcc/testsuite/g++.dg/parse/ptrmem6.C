//PR C++/27805

struct A;

void foo()
{
    int A::* p;
    A a; // { dg-error "incomplete type" }
    a.*p;
} 

