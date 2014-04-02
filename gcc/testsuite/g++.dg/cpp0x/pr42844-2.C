// PR c++/42844
// { dg-do compile { target c++11 } }

struct A // { dg-message "user-provided default constructor" }
{
    int i;
    A() = default; // { dg-message "not user-provided" }
};

struct Base
{
    Base() {}
};

struct Derived : Base // { dg-message "user-provided default constructor" }
{
    int i;
    Derived() = default; // { dg-message "not user-provided" }
};

struct Derived2 : Base // { dg-message "user-provided default constructor" }
{
    int i;
    Derived2() = default; // { dg-message "not user-provided" }
    Derived2( Derived2 const& ) = default;
};

struct Derived3 : Base // { dg-message "user-provided default constructor" }
{
    int i;
    Derived3( Derived3 const& ) = default;
    Derived3() = default; // { dg-message "not user-provided" }
};

void f()
{
    const A a; // { dg-error "uninitialized const" }
    const Derived d; // { dg-error "uninitialized const" }
    const Derived2 d2; // { dg-error "uninitialized const" }
    const Derived3 d3; // { dg-error "uninitialized const" }
}
