// PR c++/52292
// { dg-options -std=c++11 }

template <template <typename...> class T>
struct foo {
    template <typename... U>
    foo(T<U...> x) { }
};

template <typename T>
struct bar {
    bar(T x) : value(x) { }

    T value;
};

struct generic : private foo<bar> {
    template <typename T>
    generic(bar<T> x) : foo(x)
    {
    }

};

int main()
{
    bar<int> x(32);
    generic y(x); // FAILS
}
