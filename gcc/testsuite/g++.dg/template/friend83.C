// PR c++/115616
// { dg-do compile { target c++20 } }

template <int X, int Y> void bar() {}

template <typename T>
struct Reader
{
    template <int X>
    friend void foo(Reader<T>);
};

template <typename T, int Y>
struct Writer
{
    template <int X>
    friend void foo(Reader<T>) {bar<X, Y>();}
};

int main()
{
    foo<10>(Reader<int>{});
    Writer<int, 20>{};
}
