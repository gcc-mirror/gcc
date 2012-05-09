// { dg-options "-std=c++11 -pedantic-errors" }
// { dg-prune-output "invalid" }

template<int>struct A{};
template<class...U>void f(U...){
    A<sizeof...U> x; // { dg-error "surrounded by parentheses" }
}


template<int...> struct Indices;
template<class> struct Next_increasing_indices;
template<int...I> struct Next_increasing_indices<Indices<I...> > {
    typedef Indices<I...,sizeof...I> type; // { dg-error "surrounded by parentheses" }
};
