// { dg-options "-std=c++1z -fconcepts" }

template<typename T1, typename T2>
concept bool C1 = true;

template<typename T1, typename T2, typename T3>
concept bool C2 = true;


template<C1 T> // { dg-error "not a type" }
constexpr bool f1( )  { return true; }

template<C2<int> T> // { dg-error "expected" }
constexpr bool f2( )  { return true; }
