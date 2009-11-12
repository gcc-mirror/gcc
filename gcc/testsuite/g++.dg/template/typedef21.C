// PR c++/37037

typedef void F(void);
template <typename T> struct S 
{
    static F f;
};
template class S<int>;
template <class T> void S<T>::f(void)
{}

