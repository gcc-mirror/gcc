// PR c++/68049
// { dg-do compile { target c++11 } }

template <typename T> struct Bar
{
    using type = T;
};
template <typename T> struct Foo
{
    typedef typename Bar<T>::type alias_type [[gnu::may_alias]];

    alias_type operator()() { return {}; }
};

template <typename T> void print(T) {}

int main()
{
    print(Foo<int>()());
    print(0);
    return 0;
}
