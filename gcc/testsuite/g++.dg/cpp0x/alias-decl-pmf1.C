// PR c++/67339
// { dg-do compile { target c++11 } }

template < typename T>
struct A
{
    void foo();
    template < typename S, typename W >
        using N = void (T::*)(S, W) const ;
};

template < typename T>
void A<T>::foo()
{
    typename A<T>::template N<int, int> fun = &T::out;
}
