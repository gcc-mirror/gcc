// PR c++/69958
// { dg-do compile { target c++11 } }

typedef decltype(sizeof(int)) size_t;

template <typename...Ts>
struct list { };

template <size_t N>
struct size {  };

template <typename...Ts>
using size_for = size<sizeof...(Ts)>;

template<class T, class U> struct assert_same;
template<class T> struct assert_same<T,T> {};

template <typename T, typename...Ts>
using wrapped = list<T, size_for<T, Ts...>>;

// This assertion fails (produces size<4>)
assert_same<
    list<float, size<5>>,
    wrapped<float, int, double, char, unsigned>> a3;


template <typename T, typename...Ts>
using wrapped2 = list<T, size_for<Ts..., T>>;

// This assertion fails (produces size<2>)
assert_same<
    list<float, size<5>>,
    wrapped2<float, int, double, char, unsigned>> a4;
