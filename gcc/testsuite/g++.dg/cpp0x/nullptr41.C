// PR c++/89212
// { dg-do compile { target c++11 } }

template <int, typename T> using enable_if_t = int;

template<typename U, typename W, typename Y, class X, W(X::*foo)() = nullptr>
struct p
{
    template<U(Y::*fun)() = foo, typename T = enable_if_t<nullptr == fun, int>>
    p(T) { }
    p() = default;
};

struct A
{
    p<void, void, A, A> i = 1;
    void bar();
    p<void, void, A, A, &A::bar> j;
};
