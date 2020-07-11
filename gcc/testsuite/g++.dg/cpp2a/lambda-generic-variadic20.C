// PR c++/94546
// { dg-do compile { target c++20 } }

template <class T> T&& forward(T&& t) { return static_cast<T&&>(t); }

template <class X>
void test(X&& plot)
{
    // Note: For brevity, this lambda function is only
    // defined, not called nor assigned to a variable.
    // Doing those things won't fix the error.
    [&]<class... T>(T&&... rest)
    {
        plot(forward<T>(rest)...);
    };
}
int main()
{
    auto Plot = [](auto&&...)
    {
    };
    test(Plot);
}
