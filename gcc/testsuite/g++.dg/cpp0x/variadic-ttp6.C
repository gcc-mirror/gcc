// { dg-do compile { target c++11 } }

template <class T> struct A { using type = T; };
template <template <class...> class C, class... Ts>
struct A<C<Ts...>> { };

template <class T2, template <class> class TT> struct B { };
template <class T3> struct C { };

A<B<int,C>>::type a;
