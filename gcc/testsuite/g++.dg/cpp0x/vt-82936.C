// PR c++/82936
// { dg-do compile { target c++11 } }

int fun(int i)
{
    return 0;
}
template <typename F>
struct outer;
template <typename R, typename ...Args>
struct outer<R(Args...)>
{
    template <R(& f)(Args...)>
    struct callable
    {
    };
};
outer<int(int)>::callable<fun> f;
