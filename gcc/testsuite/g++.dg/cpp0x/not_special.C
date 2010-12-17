// I, Howard Hinnant, hereby place this code in the public domain.

// Test that move constructor and move assignement are special.
//   That is, their presence should inhibit compiler generated
//   copy ctor or assignment.

// { dg-options "-std=c++0x" }

#include <assert.h>

template <bool> struct sa;
template <> struct sa<true> {};

struct one   {char x[1];};
struct two   {char x[2];};

int copy = 0;
int assign = 0;

struct base
{
    base() {}
    base(const base&) {++copy;}
    base& operator=(const base&) {++assign; return *this;}
};

struct derived
    : base
{
    derived() {}
    derived(derived&&) {}			  // { dg-error "argument 1" }
    derived& operator=(derived&&) {return *this;} // { dg-error "argument 1" }
};

int test1()
{
    derived d;
    derived d2(static_cast<derived&&>(d));  // should not call base::(const base&)
    assert(copy == 0);
    derived d3(d);		// { dg-error "lvalue" }
    assert(copy == 1);
    d2 = static_cast<derived&&>(d);         // should not call base::operator=
    assert(assign == 0);
    d3 = d;			// { dg-error "lvalue" }
    assert(assign == 1);
    return 0;
}

int main()
{
    return test1();
}
