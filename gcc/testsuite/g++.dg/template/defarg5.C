// { dg-do compile }

// Origin: Ivan Godard <igodard@pacbell.net>
//	   Wolfgang Bangerth <bangerth@dealii.org>

// PR c++/17344: Substitution failure is not an error
// for default template argument

template <class> struct intTraits; 
 
template<> struct intTraits<int> { 
    static const int i = 0; 
}; 
 
template<typename E, E i = intTraits<E>::i> struct A {}; 
 
struct S { 
    template <template <typename> class X> S(X<void>); 
}; 
 
int bar(S); 
int bar(A<int,0>); 
 
A<int> bed; 
int i = bar(bed); 
