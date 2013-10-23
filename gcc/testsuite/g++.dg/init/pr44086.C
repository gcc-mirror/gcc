// PR c++/44086
// { dg-do compile }

struct A		// { dg-error "uninitialized" "" { target c++11 } }
{
    int const i : 2; // { dg-message "should be initialized" "" { target c++98 } }
};

void f()
{
    A a;		      // { dg-error "deleted|uninitialized const" }
    new A;		      // { dg-error "deleted|uninitialized const" }
    A();		      // { dg-error "deleted" "" { target c++11 } }
    new A();		      // { dg-error "deleted" "" { target c++11 } }
}
