// { dg-do compile }

// Origin: David Abrahams <dave@boost-consulting.com>
//	   Wolfgang Bangerth <bangerth@ticam.utexas.edu>

// PR c++/12170: Deducing template template parameter from nested
// class template.

template <typename> struct W {};

template< template<typename> class F, typename T>
int foo(W< F<T> >);


template<typename T>
struct L  {
    static int const value = sizeof(foo(W<T>()));
    typedef T type;
};


template <typename>
struct Y {
    template <typename> struct X { typedef int type; };
    typedef typename L<X<int> >::type type;
};

template struct Y<int>;
