// PR c++/33964

template<typename T>
struct X { };

template<typename T>
struct X<typename T::foo> { }; // { dg-error "not deducible|T" }

template<int N>
struct X<int[N]> {}; // okay


template<typename T, typename T::foo V>
struct Y { };

template<typename T, typename U, U v>
struct Y<T, v> { }; // { dg-error "" "" { target { ! c++17 } } }


template<typename T, T V>
struct Z { };

template<typename T>
struct Z<T, (T)0> { }; // { dg-error "13:template argument" }
