// PR c++/56285
// { dg-options -std=c++11 }

struct foo {
    explicit foo(int&&) {}
};

struct bar: private foo {
    using foo::foo;
};

int main()
{
    bar b { 42 };
}
