// PR c++/48292
// { dg-options -std=c++0x }

template <typename... Args> int g(Args...);

template <int N = 0>
struct A
{
    template <typename... Args>
    static auto f(Args... args) -> decltype(g(args...));
};

int main()
{
    A<>::f();
    return 0;
}
