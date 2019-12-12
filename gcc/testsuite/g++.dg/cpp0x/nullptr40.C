// PR c++/89212
// { dg-do compile { target c++11 } }

template <int, typename T> using enable_if_t = int;

template<class X, void(X::*foo)() = nullptr>
struct p
{
    template<void(X::*fun)() = foo, typename T = enable_if_t<nullptr == fun, int>>
    p(T) { }
    p() = default;
};

struct A
{
    p<A> i = 1;
    void bar();
    p<A, &A::bar> j;
};
