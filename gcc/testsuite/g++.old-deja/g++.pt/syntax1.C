struct X {
    enum {
       a = 0,
       n = 0
    };
};

template<class T1, class T2>
struct Y {

    enum {
       a = T1::a + T2::a,

       n = meta_max<T1::n,T2::n>::max // Crash here.
    };
};

int z = Y<X,X>::a;
