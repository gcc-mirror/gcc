// PR c++/27714

template<typename> struct A
{
    static void* operator new(__SIZE_TYPE__);
    template<typename T> friend void* A<T>::operator new(__SIZE_TYPE__);
};

A<int> a;
