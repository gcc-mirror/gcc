struct A {};

struct B : public A
{
    static void foo ();
};

template <typename T> struct C
{
    C() : f(B::foo) {}
    void (*f)();
};

C<int> c;
