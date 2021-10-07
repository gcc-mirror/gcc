template<class T> struct A{
        typedef T& Type;
};
template<class T> void f(const typename A<T>::Type){}
template <> void f<int>(const typename A<int>::Type){}
