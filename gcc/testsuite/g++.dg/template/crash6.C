template <class> struct A { static const int n = 1; } ;
template <int> struct B;

template <class S>
struct restype_order {
    static const int s = A<S>::n;
    typedef typename B<(s > 0)>::t t;
};
