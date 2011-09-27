// PR c++/46105

template< typename T >
struct empty { // support class is like stripped-down enable_if
    typedef void type;
};

template< class T, typename v = void > // v is always void!
struct element {
    typedef typename T::value_type type;
};

template< class T > // T in deduced context, T::element_type is SFINAE:
struct element< T, typename empty< typename T::element_type >::type > {
    typedef typename T::element_type type;
};

template< class T >
struct element< T const, typename empty< typename T::element_type >::type > {
    typedef typename T::element_type const type;
};

struct has_et {
    typedef int element_type;
};

element<has_et const>::type ip = 0;
