// PR c++/18121

// We were trying to layout the array
// type but since the type/value of A<N>::i
// was not known at template declation type,
// we were crashing

template<int> struct A
{
    static int const i = 1;
};

template<int N> struct B
{
    typedef int (*p)[A<N>::i];
};
