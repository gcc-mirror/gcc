// { dg-do compile }

template<int size>
struct Foobar {
    // Contents irrelevant
};

template <typename A>
struct Wrapper {
    // Contents irrelevant
};

template <typename A>
Foobar<sizeof(Wrapper<A>)> *
compiler_bug (A)
{
    return 0;
}

int main()
{
    compiler_bug(1);
}
