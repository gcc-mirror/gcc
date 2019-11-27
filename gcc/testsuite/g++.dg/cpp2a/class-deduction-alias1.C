// Testcase from P1814R0
// { dg-do compile { target c++2a } }

template <class T> struct identity { using type = T; };
template <class T> using identity_t = typename identity<T>::type;
template <class T> concept Int = __is_same_as (T, int);

template <class T, class U> struct C {
  C(T, U);			// #1 { dg-message "constraint" }
};
template<class T, class U>
C(T, U) -> C<T, identity_t<U>>; // #2 { dg-message "constraint" }

template<class V>
using A = C<V *, V *>;

template<Int W>
using B = A<W>;
    
int i{};
double d{};
A a1(&i, &i); // { dg-bogus "" "Deduces A<int>" }
A a2(i, i);   // { dg-error "" "cannot deduce V * from i" }
A a3(&i, &d); // { dg-error "" } #1: Cannot deduce (V*, V*) from (int *, double *) 
                              // #2: Cannot deduce A<V> from C<int *, double *>
B b1(&i, &i); // { dg-bogus "" "Deduces B<int>" }
B b2(&d, &d); // { dg-error "" "cannot deduce B<W> from C<double *, double *>" }
