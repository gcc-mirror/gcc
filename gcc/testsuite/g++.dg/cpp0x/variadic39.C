// { dg-do compile { target c++11 } }
template<typename... Args>
struct tuple {};

template<typename T, typename... Args>
struct tuple<Args..., T> { }; // { dg-error "end" }


template<int... Values>
struct int_vec { };

template<int I, int... Values>
struct int_vec<Values..., I> { }; // { dg-error "end" }
