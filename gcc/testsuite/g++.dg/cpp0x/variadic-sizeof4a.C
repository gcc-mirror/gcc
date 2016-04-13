// PR c++/69958
// { dg-do compile { target c++11 } }

typedef decltype(sizeof(int)) size_t;

template <typename...Ts>
struct list { };

template <size_t N>
struct size {  };

template <unsigned...Ts>
using size_for = size<sizeof...(Ts)>;

template<class T, class U> struct assert_same;
template<class T> struct assert_same<T,T> {};

template <typename T, unsigned...Ts>
using wrapped = list<T, size_for<0, Ts...>>;

// This assertion fails (produces size<4>)
assert_same<
    list<float, size<5>>,
  wrapped<float,2,3,4,5>> a3;


template <typename T, unsigned...Ts>
using wrapped2 = list<T, size_for<Ts..., 0>>;

// This assertion fails (produces size<2>)
assert_same<
    list<float, size<5>>,
  wrapped2<float,2,3,4,5>> a4;
