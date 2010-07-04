// PR c++/43868
// { dg-options "-g" }

struct Foo
{
    virtual void do_something() = 0;
};

template <typename T>
struct Foo_impl;

template <typename R, typename O>
struct Foo_impl<R (O::*)() const> : public Foo
{
    struct Helper
    {
        typedef int Some_type;
        operator Some_type () const { return 0; }
        Helper( R (O::*)() const) {}
    };

    void do_something() {  Helper( 0); };
};

void register_foo_internal( Foo*) {};

template <typename TT>
void register_foo( TT) { register_foo_internal( new Foo_impl<TT>()); }

struct Bar
{
};

void setup()
{
    register_foo( (int (Bar::*) () const) 0);
}
