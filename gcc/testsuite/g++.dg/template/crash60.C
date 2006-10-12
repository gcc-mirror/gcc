//PR c++/27961

struct A
{
    template<int> void foo(X);  // { dg-error "declared" }
};

template<int> void f()(0);      // { dg-error "initialized" }
 
