// PR c++/90215
// { dg-do compile { target c++17 } }

struct X
{
    template <class F>
    void f(F f)
    {
        f(0);
    }
};

template <class... Xs>
void bug(Xs... xs)
{
    int i;

    [&](auto&... ys)
    {
        (xs.f([&](auto)
        {
            ys;
        }), ...);
    }(i);
}

int main()
{
    bug(X{});
}
