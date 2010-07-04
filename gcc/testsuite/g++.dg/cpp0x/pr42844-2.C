// PR c++/42844
// { dg-do compile }
// { dg-options "-std=c++0x" }

struct A // { dg-message "user-provided default constructor" }
{
    A() = default; // { dg-message "not user-provided" }
};

struct Base
{
    Base() {}
};

struct Derived : Base // { dg-message "user-provided default constructor" }
{
    Derived() = default; // { dg-message "not user-provided" }
};

struct Derived2 : Base // { dg-message "user-provided default constructor" }
{
    Derived2() = default; // { dg-message "not user-provided" }
    Derived2( Derived2 const& ) = default;
};

struct Derived3 : Base // { dg-message "user-provided default constructor" }
{
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
