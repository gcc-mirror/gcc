// PR c++/34950

template <class T = int> struct policy {
    typedef int unnecessary;
};

template <class Policy> struct A {
    typedef int type;
    typedef typename Policy::unnecessary unused;
};

template <class T> struct S {
    typedef int type;
    typedef typename A<T>::type unused;
};

template <class, class T> typename S<T>::type         foo();
template <class>                   S<policy<> >::type foo();

template <typename T> int def(T);
const int i = def(foo<int>);
