// PR c++/78018
// { dg-do compile { target c++14 } }

struct A
{
    void f1();

    template <typename F>
    void f2(F f);

    template<typename T>
    void f3(T t);
};

struct B
{
    template<typename L>
    void f(L) { }
};

void A::f1()
{
    f2([&] (auto t) { f3(t); } );
}

template <typename F>
void A::f2(F f)
{
    B b;
    f(b);
}

template<typename T>
void A::f3(T t)
{
}

