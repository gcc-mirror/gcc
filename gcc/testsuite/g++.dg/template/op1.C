template <class T> struct X {
    typedef int type;
};

template <class T> struct O {
    struct I {
        operator typename X<T>::type ();
    };
};

template <class T>
O<T>::I::operator typename X<T>::type () {}
