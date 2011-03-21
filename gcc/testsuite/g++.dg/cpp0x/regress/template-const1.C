// PR c++/47897
// { dg-options -std=c++0x }

template < typename T, T N >
struct S
{
    static const T value = N;
    typedef S< T, value + 1 > next;
};
