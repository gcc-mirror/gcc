// PR c++/44086
// { dg-do compile }

struct A
{
    int const i : 2; // { dg-message "should be initialized" }
};

void f()
{
    A a;    // { dg-error "uninitialized const" }
    new A;  // { dg-error "uninitialized const" }
    A();
    new A();
}
