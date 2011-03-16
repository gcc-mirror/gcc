// PR c++/48115

template<typename> struct templ { };

template<typename T> T declval();

typedef int (*F2)(...);

template<int> struct Int { };

template<typename F, typename T>
struct S
{
    template<typename A>
        Int<sizeof( declval<F>()(T()) )>
        f(A);
};

int main()
{
    S<F2, templ<int> >().f(0);
}
