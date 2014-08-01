// PR c++/60241

template <typename T>
struct x
{
    template <typename U>
    struct y
    {
        typedef T result2;
    };

    typedef y<int> zy;
};

template<>
template<class T>
struct x<int>::y
{
    typedef double result2;
};

int main()
{
    x<int>::zy::result2 xxx;
    x<int>::y<int>::result2 xxx2;
}
