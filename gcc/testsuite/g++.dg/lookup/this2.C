// PR c++/106969
// { dg-do compile { target c++11 } }

struct Context
{
    void
    action() const
    {
        struct Foo
        {
            int wrapped;
            decltype(&wrapped) get() { return &wrapped; }
        } t;

        *t.get()= 42; // OK, get() returns int* not const int*

        struct Bar
        {
            using type = decltype(this); // { dg-error "invalid use of 'this'" }
        };
    }
};
