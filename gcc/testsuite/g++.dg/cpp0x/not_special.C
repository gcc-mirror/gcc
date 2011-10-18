// I, Howard Hinnant, hereby place this code in the public domain.

// Test that move constructor and move assignement are special.
//   That is, their presence should cause compiler declared
//   copy ctor or assignment to be deleted.

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

struct derived			// { dg-message "declares a move" }
    : base
{
    derived() {}
    derived(derived&&) {}
    derived& operator=(derived&&) {return *this;}
};

int test1()
{
    derived d;
    derived d2(static_cast<derived&&>(d));  // should not call base::(const base&)
    assert(copy == 0);
    derived d3(d);		// { dg-error "deleted" }
    assert(copy == 1);
    d2 = static_cast<derived&&>(d);         // should not call base::operator=
    assert(assign == 0);
    d3 = d;			// { dg-error "deleted" }
    assert(assign == 1);
    return 0;
}

int main()
{
    return test1();
}
