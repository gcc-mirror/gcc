extern const int a;

template <const int&> class X {};

template <typename> struct Y {
    X<a> x;
};

template struct Y<int>;
