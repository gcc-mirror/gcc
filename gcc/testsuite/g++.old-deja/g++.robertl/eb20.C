// Error: ICE on 2.7.2.3 and EGCS 1.0.0.
// Build don't link:

template<int N1, int N2>
struct meta_max {
    enum { max = (N1 > N2) ? N1 : N2 };
};

struct X {
    enum {
       a = 0,
       n = 0
    };
};

template<class T1, class T2, class T3>
struct Y {

    enum {
       a = T1::a + T2::a + T3::a,
       n = meta_max<meta_max<T1::n,T2::n>::max, T3::n>::max
    };
};

template<class T>
struct Z {
    enum {
       a = T::a,
       n = T::n
    };
};

Z<Y<X,X,X> > z;
