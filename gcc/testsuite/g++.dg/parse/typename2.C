template<class T, class U>
struct UnaryReturn {
        typedef T Type_t;
};

struct foo
{
        template <class T>
        typename UnaryReturn<T, int>::Type_t
        bar();
};

template<class T>
struct UnaryReturn<T, int> {
        typedef bool Type_t;
};

