// PR c++/64455
// { dg-do compile { target c++14 } }

template<typename Type>
constexpr bool IsType = true;

template <bool b, class T> struct Test
{
};

template <class T>
struct Test<true, T>
{
        typedef T type;
};

template<class T>
struct X {
    typedef typename Test<IsType<T>,T>::type type;
};

int main()
{
   X<int>::type t;
}
