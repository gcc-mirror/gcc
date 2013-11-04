// PR c++/48113
// { dg-options -std=c++11 }

template<typename T> T declval();

struct tuple { };

struct F1
{
    void operator()(tuple, int);
};

typedef void (*F2)(tuple, int);

template<typename F, typename T>
struct Bind
{
    template<typename A,
             typename R = decltype( F()(declval<T&>(), A()) )>
    R f(A);

    template<typename A,
             typename R = decltype( F()(declval<volatile T&>(), A()) )>
    R f(A) volatile;
};

int main()
{
    Bind<F1, tuple>().f(0);  // OK
    Bind<F2, tuple>().f(0);  // ERROR, should be OK
}
